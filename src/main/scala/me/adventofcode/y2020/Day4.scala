package me.adventofcode.y2020

import me.adventofcode.DayTask
import me.adventofcode.util.IntParser
import me.adventofcode.util.extensions.AnyExtension
import me.adventofcode.util.parsers.StringExt

object Day4 extends DayTask[Int, Int](4, 2020, "Passport Processing") {

  def part1(input: Seq[String]): Int = {
    val concatenatedInput = input.mkString("\n")
    val passports = parsePassports(concatenatedInput)

    println(passports)

    passports.count(_.isRight)
  }

  def part2(input: Seq[String]): Int = 1

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
    hairColor: String,
    eyeColor: String,
    passportId: String,
    countryId: Option[String]
)

object Passport {

  def parse(rawValue: String): Either[PassportParsingError, Passport] = {
    val syntax = rawValue
      .split("[ \n]")
      .map(_.split(':'))
      .collect {
        case Array(key, value) => key -> value
      }
      .toMap

    def getFieldValue[T](name: String)(parser: String => Option[T]): Either[PassportParsingError, T] =
      for {
        raw <- syntax.get(name).toRight(PassportParsingError(s"Missing required field $name"))
        parsed <- parser.apply(raw).toRight(PassportParsingError(s"Failed to parse field $name with value $raw"))
      } yield parsed

    for {
      birthYear <- getFieldValue("byr")(Year.parse(1920, 2002))
      issueYear <- getFieldValue("iyr")(Year.parse(2010, 2020))
      expirationYear <- getFieldValue("eyr")(Year.parse(2020, 2030))
      height <- getFieldValue("hgt")(Dimension.parse)
      hairColor <- getFieldValue("hcl")(Option.apply)
      eyeColor <- getFieldValue("ecl")(Option.apply)
      passportId <- getFieldValue("pid")(Option.apply)
      countryId = syntax.get("cid")
    } yield Passport(birthYear, issueYear, expirationYear, height, hairColor, eyeColor, passportId, countryId)

  }

}

case class Year private (value: Int)

object Year {

  def parse(lowerBound: Int, upperBound: Int)(value: String): Option[Year] = {
    for {
      parsedYear <- value.optInt
      if parsedYear > lowerBound && parsedYear < upperBound
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

sealed abstract class EyeColor(code: String)

object EyeColor {

  case object Amber extends EyeColor()

//  amb blu brn gry grn hzl oth

}

case class ParserError(reason: String)

trait Parser[T] {

  def parse(raw: String): Either[List[ParserError], T]

}

object PassportParser {

  type PassPortParser = Parser[Password]

}
