package me.adventofcode.util

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import scala.util.Try

object parsers {

  def parseTimeStamp(format: String)(timestamp: String): Either[String, LocalDateTime] =
    Try {
      LocalDateTime.parse(timestamp, DateTimeFormatter.ofPattern(format))
    }.toEither.left.map(e => s"Unable to parse timestamp, reason: ${e.getMessage}")

  implicit class StringExt(str: String) {
    def parseInt: Either[String, Int] =
      Try(str.toInt)
        .toEither
        .left.map(e => s"Unable to parse $str, reason: ${e.getMessage}")

    def optInt: Option[Int] = Try(str.toInt).toOption

  }

}
