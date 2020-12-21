package me.adventofcode.y2020

import cats.implicits._
import me.adventofcode.DayTask
import me.adventofcode.util.IntParser
import me.adventofcode.util.parsers.StringExt
import me.adventofcode.y2020.IKVF.DecodingResult
import me.adventofcode.y2020.catsInstances._

object Day4 extends DayTask[Int, Int](4, 2020, "Passport Processing") {

  import decoders._

  def part1(input: Seq[String]): Int = {
    val concatenatedInput = input.mkString("\n")

    val parsedPassports = parsePassports(concatenatedInput)

    parsedPassports.count(_.isRight)
  }

  // part 2 does the same as part one
  def part2(input: Seq[String]): Int = part1(input)

  private def parsePassports(text: String): Seq[DecodingResult[Passport]] =
    text
      .split("\n\n")
      .toVector
      .map { rawObj =>
        for {
          maybePassport <- IKVF.parse[Passport](rawObj)
          passport <- maybePassport.headOption.toRight(DecodingError("Empty passport"))
        } yield passport
      }
}

case class Passport private (
    birthYear: BirthYear,
    issueYear: IssueYear,
    expirationYear: ExpirationYear,
    height: Height,
    hairColor: HairColor,
    eyeColor: EyeColor,
    passportId: PassportId,
    countryId: Option[CountyId]
)

case class CountyId(value: String)

object Year {

  def parser[T](start: Int, end: Int)(wrapper: Int => T): String => Option[T] =
    value =>
      for {
        year <- value.optInt
        if year >= start && year <= end
      } yield wrapper(year)

}

case class BirthYear private (value: Int)

object BirthYear {

  val parse: String => Option[BirthYear] = Year.parser(1920, 2002)(BirthYear.apply)

}

case class IssueYear private (value: Int)

object IssueYear {

  val parse: String => Option[IssueYear] = Year.parser(2010, 2020)(IssueYear.apply)

}

case class ExpirationYear private (value: Int)

object ExpirationYear {

  val parse: String => Option[ExpirationYear] = Year.parser(2020, 2030)(ExpirationYear.apply)

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

  import IKVF._
  import IKVF.Decoder._

  implicit val passportIdDecoder: Decoder[PassportId] = fieldValueDecoder(PassportId.parse)
  implicit val hairColorDecoder: Decoder[HairColor] = fieldValueDecoder(HairColor.parse)
  implicit val eyeColorDecoder: Decoder[EyeColor] = fieldValueDecoder(EyeColor.parse)
  implicit val heightDecoder: Decoder[Height] = fieldValueDecoder(Height.parse)
  implicit val birthYearDecoder: Decoder[BirthYear] = fieldValueDecoder(BirthYear.parse)
  implicit val issueYearDecoder: Decoder[IssueYear] = fieldValueDecoder(IssueYear.parse)
  implicit val expirationYearDecoder: Decoder[ExpirationYear] = fieldValueDecoder(ExpirationYear.parse)
  implicit val countryIdDecoder: Decoder[CountyId] = fieldValueDecoder(CountyId(_).some)

  implicit val passportDecoder: Decoder[Passport] =
    (
      fieldDecoder[BirthYear]("byr"),
      fieldDecoder[IssueYear]("iyr"),
      fieldDecoder[ExpirationYear]("eyr"),
      fieldDecoder[Height]("hgt"),
      fieldDecoder[HairColor]("hcl"),
      fieldDecoder[EyeColor]("ecl"),
      fieldDecoder[PassportId]("pid"),
      optionalFieldDecoder[CountyId]("cid")
    ).mapN(Passport)

}
