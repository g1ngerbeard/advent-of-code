package me.adventofcode.y2018.util

import scala.util.Try

object Parsers {

  implicit class StringExt(str: String) {
    def parseInt: Either[String, Int] =
      Try(str.toInt)
        .toEither
        .left.map(e => s"Unable to parse $str, reason: ${e.getMessage}")
  }

}
