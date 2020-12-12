package me.adventofcode.y2020

import me.adventofcode.DayTask
import me.adventofcode.util.IntParser
import me.adventofcode.util.extensions.{AnyExtension, BoolExtension}
import me.adventofcode.util.parsers.StringExt

object Day4 extends DayTask[Int, Int](4, 2020, "Passport Processing") {

  def part1(input: Seq[String]): Int = {
    val concatenatedInput = input.mkString("\n")
    val passports = parsePassports(concatenatedInput)

    println(passports)

    passports.count(_.isRight)
  }

  // part 2 does the same as part one
  def part2(input: Seq[String]): Int = part1(input)

  private def parsePassports(text: String): Seq[Either[PassportParsingError, Passport]] =
    text
      .split("\n\n")
      .map(Passport.parse)

}

case class PassportParsingError(reason: String)

case class Passport(
    birthYear: Year,
    issueYear: Year,
    expirationYear: Year,
    height: Dimension,
    hairColor: HairColor,
    eyeColor: EyeColor,
    passportId: PassportId,
    countryId: Option[String]
)

object Passport {

  def parse(rawValue: String): Either[PassportParsingError, Passport] = {
    val tokenMap = rawValue
      .split("[ \n]")
      .map(_.split(':'))
      .collect {
        case Array(key, value) => key -> value
      }
      .toMap

    def getFieldValue[T](name: String)(parser: String => Option[T]): Either[PassportParsingError, T] =
      for {
        raw <- tokenMap.get(name).toRight(PassportParsingError(s"Missing required field $name"))
        parsed <- parser.apply(raw).toRight(PassportParsingError(s"Failed to parse field $name with value $raw"))
      } yield parsed

    for {
      birthYear <- getFieldValue("byr")(Year.parse(1920, 2002))
      issueYear <- getFieldValue("iyr")(Year.parse(2010, 2020))
      expirationYear <- getFieldValue("eyr")(Year.parse(2020, 2030))
      height <- getFieldValue("hgt")(Dimension.parse)
      hairColor <- getFieldValue("hcl")(HairColor.parse)
      eyeColor <- getFieldValue("ecl")(EyeColor.parse)
      passportId <- getFieldValue("pid")(PassportId.parse)
      countryId = tokenMap.get("cid")
    } yield Passport(birthYear, issueYear, expirationYear, height, hairColor, eyeColor, passportId, countryId)

  }

}

case class Year private (value: Int)

object Year {

  def parse(lowerBound: Int, upperBound: Int)(value: String): Option[Year] = {
    for {
      parsedYear <- value.optInt
      if parsedYear >= lowerBound && parsedYear <= upperBound
    } yield Year(parsedYear)
  }

}

case class Dimension(value: Int, unit: DimensionUnit)

object Dimension {

  def parse(rawValue: String): Option[Dimension] =
    rawValue match {
      case s"${IntParser(value)}cm" if value >= 150 && value <= 193 =>
        Dimension(value, DimensionUnit.Centimeters).some
      case s"${IntParser(value)}in" if value >= 59 && value <= 76 =>
        Dimension(value, DimensionUnit.Inches).some
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
