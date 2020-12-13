package me.adventofcode.y2020

import cats.Monad
import cats.implicits._
import me.adventofcode.DayTask
import me.adventofcode.util.IntParser
import me.adventofcode.util.parsers.StringExt
import me.adventofcode.y2020.IKVF.{Field, IKVFArray, IKVFObject, IKVFValue}
import me.adventofcode.y2020.IKVFDecoder.{fieldDecoder, fieldValueDecoder, optionalFieldDecoder}
import me.adventofcode.y2020.PassportProblem.ConstraintViolated
import me.adventofcode.y2020.catsInstances._
import me.adventofcode.y2020.decoders._

object Day4 extends DayTask[Int, Int](4, 2020, "Passport Processing") {

  def part1(input: Seq[String]): Int = {
    val concatenatedInput = input.mkString("\n")

    val parsedPassports = parsePassports(concatenatedInput)

    parsedPassports.count(_.isRight)
  }

  // part 2 does the same as part one
  def part2(input: Seq[String]): Int = part1(input)

  private def parsePassports(text: String): Seq[Either[IKVFProblem, Passport]] =
    text
      .split("\n\n")
      .toSeq
      .map(IKVF.parse[Passport])

}

sealed trait PassportProblem

object PassportProblem {

  case class ConstraintViolated(reason: String) extends PassportProblem

}

case class Passport private (
    birthYear: Year,
    issueYear: Year,
    expirationYear: Year,
    height: Height,
    hairColor: HairColor,
    eyeColor: EyeColor,
    passportId: PassportId,
    countryId: Option[CountyId]
)

object Passport {

  def make(
      birthYear: Year,
      issueYear: Year,
      expirationYear: Year,
      height: Height,
      hairColor: HairColor,
      eyeColor: EyeColor,
      passportId: PassportId,
      countryId: Option[CountyId]
  ): Either[ConstraintViolated, Passport] = {
    def checkBounds(startInc: Int, endInc: Int, year: Year, name: String): Either[ConstraintViolated, Unit] =
      if (startInc to endInc contains year.value)
        ().asRight
      else
        ConstraintViolated(s"$name $year is out of bounds").asLeft

    for {
      _ <- checkBounds(1920, 2002, birthYear, "Birth year")
      _ <- checkBounds(2010, 2020, issueYear, "Issue year")
      _ <- checkBounds(2020, 2030, expirationYear, "Expiration year")
    } yield
      Passport(
        birthYear,
        issueYear,
        expirationYear,
        height,
        hairColor,
        eyeColor,
        passportId,
        countryId
      )
  }

}

case class CountyId(value: String)

case class Year private (value: Int)

object Year {

  def parse(value: String): Option[Year] = value.optInt.map(Year(_))

}

case class Height(value: Int, unit: DimensionUnit)

object Height {

  def parse(rawValue: String): Option[Height] =
    rawValue match {
      case s"${IntParser(value)}cm" if value >= 150 && value <= 193 =>
        Height(value, DimensionUnit.Centimeters).some
      case s"${IntParser(value)}in" if value >= 59 && value <= 76 =>
        Height(value, DimensionUnit.Inches).some
      case _ => None
    }

}

sealed trait DimensionUnit

object DimensionUnit {

  case object Centimeters extends DimensionUnit

  case object Inches extends DimensionUnit

}

sealed abstract class EyeColor(val code: String)

object EyeColor {

  case object Amber extends EyeColor("amb")
  case object Blue extends EyeColor("blu")
  case object Brown extends EyeColor("brn")
  case object Grey extends EyeColor("gry")
  case object Green extends EyeColor("grn")
  case object Hazel extends EyeColor("hzl")
  case object Other extends EyeColor("oth")

  val all = Set(Amber, Blue, Brown, Grey, Green, Hazel, Other)

  val parse: String => Option[EyeColor] = all.map(v => v.code -> v).toMap.get

}

case class HairColor private (value: String)

object HairColor {

  private val Regex = "#[0-9a-f]{6}".r

  def parse(value: String): Option[HairColor] =
    Regex
      .matches(value)
      .guard[Option]
      .map(_ => HairColor(value))

}

case class PassportId private (value: String)

object PassportId {

  private val Regex = "[0-9]{9}".r

  def parse(value: String): Option[PassportId] =
    Regex
      .matches(value)
      .guard[Option]
      .map(_ => PassportId(value))

}

object decoders {

  implicit val passportIdDecoder: IKVFDecoder[PassportId] = fieldValueDecoder(PassportId.parse)
  implicit val hairColorDecoder: IKVFDecoder[HairColor] = fieldValueDecoder(HairColor.parse)
  implicit val eyeColorDecoder: IKVFDecoder[EyeColor] = fieldValueDecoder(EyeColor.parse)
  implicit val heightDecoder: IKVFDecoder[Height] = fieldValueDecoder(Height.parse)
  implicit val yearDecoder: IKVFDecoder[Year] = fieldValueDecoder(Year.parse)
  implicit val countryIdDecoder: IKVFDecoder[CountyId] = fieldValueDecoder(CountyId(_).some)

  implicit val passportDecoder: IKVFDecoder[Passport] =
    (
      fieldDecoder[Year]("byr"),
      fieldDecoder[Year]("iyr"),
      fieldDecoder[Year]("eyr"),
      fieldDecoder[Height]("hgt"),
      fieldDecoder[HairColor]("hcl"),
      fieldDecoder[EyeColor]("ecl"),
      fieldDecoder[PassportId]("pid"),
      optionalFieldDecoder[CountyId]("cid")
    ).mapN(Passport.make).flatMap(result => _ => result.leftMap(e => DecodingError(e.toString)))

}

// ================================================

sealed trait IKVFProblem

case class DecodingError(reason: String) extends IKVFProblem

// IKVF = imaginary key value format
object IKVF {

  sealed trait IKVFValue

  case class Field(name: String, value: String) extends IKVFValue

  case class IKVFObject(fields: Map[String, Field]) extends IKVFValue

  case class IKVFArray(objects: Seq[IKVFObject]) extends IKVFValue

  // todo: return Seq since valid IKVF is an array of values
  def parse[T: IKVFDecoder](raw: String): Either[IKVFProblem, T] =
    for {
      ast <- buildObjectAst(raw)
      result <- IKVFDecoder[T].decode(ast)
    } yield result

  // todo: use parser combinators?
  private def buildObjectAst(raw: String): Either[IKVFProblem, IKVFValue] =
    raw
      .split("[ \n]")
      .toVector
      .traverse {
        _.split(':').toVector match {
          case Vector(key, value) => Right(Field(key, value))
          case invalid            => Left(DecodingError(s"Invalid field format: $invalid"))
        }
      }
      .map { fields =>
        IKVFObject(fields.map(f => f.name -> f).toMap)
      }

}

object IKVFDecoder {

  def apply[T: IKVFDecoder]: IKVFDecoder[T] = implicitly[IKVFDecoder[T]]

  implicit def sequenceDecoder[T: IKVFDecoder]: IKVFDecoder[Seq[T]] = {
    case IKVFArray(objects) => objects.traverse(IKVFDecoder[T].decode)
    case unexpected         => DecodingError(s"Expected IKVFArray got ${unexpected.getClass}").asLeft
  }

  def optionalFieldDecoder[T: IKVFDecoder](name: String): IKVFDecoder[Option[T]] = {
    case IKVFObject(fields) => fields.get(name).traverse(IKVFDecoder[T].decode)
    case unexpected         => DecodingError(s"Expected IKVFObject got ${unexpected.getClass}").asLeft
  }

  def fieldDecoder[T: IKVFDecoder](name: String): IKVFDecoder[T] = {
    case IKVFObject(fields) =>
      for {
        field <- fields.get(name).toRight(DecodingError(s"Required field $name is missing"))
        parsedValue <- IKVFDecoder[T].decode(field)
      } yield parsedValue
    case unexpected => DecodingError(s"Expected IKVFObject got ${unexpected.getClass}").asLeft
  }

  def fieldValueDecoder[T](p: String => Option[T]): IKVFDecoder[T] = {
    case Field(name, value) => p(value).toRight(DecodingError(s"Invalid value for the field $name: $value"))
    case unexpected         => DecodingError(s"Expected Field got ${unexpected.getClass}").asLeft
  }

}

trait IKVFDecoder[T] {

  def decode(value: IKVFValue): Either[IKVFProblem, T]

}

object catsInstances {

  implicit val decoderMonad: Monad[IKVFDecoder] = new Monad[IKVFDecoder] {

    def flatMap[A, B](fa: IKVFDecoder[A])(f: A => IKVFDecoder[B]): IKVFDecoder[B] =
      (obj: IKVFValue) =>
        for {
          a <- fa.decode(obj)
          b <- f(a).decode(obj)
        } yield b

    //fixme: this is not tailrec ;(
    def tailRecM[A, B](a: A)(f: A => IKVFDecoder[Either[A, B]]): IKVFDecoder[B] = { (obj: IKVFValue) =>
      f(a)
        .decode(obj)
        .flatMap(_.leftFlatMap(tailRecM(_)(f).decode(obj)))
    }

    def pure[A](x: A): IKVFDecoder[A] = _ => x.asRight

  }

}
