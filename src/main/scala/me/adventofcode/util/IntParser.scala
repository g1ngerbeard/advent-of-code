package me.adventofcode.util

import me.adventofcode.util.parsers.StringExt

object IntParser {

  def unapply(arg: String): Option[Int] = arg.optInt

}