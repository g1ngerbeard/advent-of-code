package me.adventofcode.y2020

import me.adventofcode.DayTask
import me.adventofcode.util.IntParser
import me.adventofcode.util.extensions.{AnyExtension, BoolExtension}
import me.adventofcode.util.parsers.StringExt
import me.adventofcode.y2020.PassportProblem.{ConstraintViolated, ParsingFailed}

object Day4 extends DayTask[Int, Int](4, 2020, "Passport Processing") {

  def part1(input: Seq[String]): Int = {
    val concatenatedInput = input.mkString("\n")

    val parsedPassports = parsePassports(concatenatedInput)

    parsedPassports.count(_.isRight)
  }

  // part 2 does the same as part one
  def part2(input: Seq[String]): Int = part1(input)

  private def parsePassports(text: String): Seq[Either[PassportProblem, Passport]] =
    text
      .split("\n\n")
      .map(Passport.parse)

}

sealed trait PassportProblem

object PassportProblem {

  case class ParsingFailed(reason: String) extends PassportProblem

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

  def parse(rawValue: String): Either[PassportProblem, Passport] = {
    val tokenMap = rawValue
      .split("[ \n]")
      .map(_.split(':'))
      .collect {
        case Array(key, value) => key -> value
      }
      .toMap

    def getFieldValue[T](name: String)(parser: String => Option[T]): Either[ParsingFailed, T] =
      for {
        raw <- tokenMap.get(name).toRight(ParsingFailed(s"Missing required field $name"))
        parsed <- parser.apply(raw).toRight(ParsingFailed(s"Failed to parse field $name with value $raw"))
      } yield parsed

    for {
      birthYear <- getFieldValue("byr")(Year.parse)
      issueYear <- getFieldValue("iyr")(Year.parse)
      expirationYear <- getFieldValue("eyr")(Year.parse)
      height <- getFieldValue("hgt")(Height.parse)
      hairColor <- getFieldValue("hcl")(HairColor.parse)
      eyeColor <- getFieldValue("ecl")(EyeColor.parse)
      passportId <- getFieldValue("pid")(PassportId.parse)
      countryId = tokenMap.get("cid").map(CountyId)
      validPassport <- Passport.make(
        birthYear,
        issueYear,
        expirationYear,
        height,
        hairColor,
        eyeColor,
        passportId,
        countryId
      )
    } yield validPassport
  }

  def make(
      birthYear: Year,
      issueYear: Year,
      expirationYear: Year,
      height: Height,
      hairColor: HairColor,
      eyeColor: EyeColor,
      passportId: PassportId,
      countryId: Option[CountyId]
  ): Either[PassportProblem, Passport] = {

    def checkBounds(
        startInc: Int,
        endInc: Int,
        year: Year,
        name: String
    ): Either[ConstraintViolated, Unit] =
      Either.cond(
        startInc to endInc contains year.value,
        (),
        ConstraintViolated(s"$name $year is out of bounds")
      )

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

  def parse(value: String): Option[Year] =
    for {
      year <- value.optInt
      // check if year contains 4 digits
      if year >= 1000 && year <= 9999
    } yield Year(year)

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
      .guardOpt
      .map(_ => HairColor(value))

}

case class PassportId private (value: String)

object PassportId {

  private val Regex = "[0-9]{9}".r

  def parse(value: String): Option[PassportId] =
    Regex
      .matches(value)
      .guardOpt
      .map(_ => PassportId(value))

}
