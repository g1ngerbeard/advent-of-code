package me.adventofcode.y2018

object Day5 {

  val UnitTypes: Set[Char] = ('a' to 'z').toSet

  val reactions: Set[String] = UnitTypes.map(_.toString).flatMap(c => Set(c + c.toUpperCase, c.toUpperCase + c))

  def reduce(polymer: String): String =
    reactions
      .find(polymer.contains)
      .fold(polymer)(pair => reduce(polymer.replace(pair, "")))

  def shortestReduction(polymer: String): String =
    UnitTypes
      .map(unitType => polymer.filterNot(c => c == unitType || c == unitType.toUpper))
      .map(reduce)
      .minBy(_.length)


}
