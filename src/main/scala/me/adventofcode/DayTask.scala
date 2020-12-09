package me.adventofcode

abstract class DayTask[R1, R2](val day: Int, val year: Int, val name: String) {

  def part1(input: Seq[String]): R1

  def part2(input: Seq[String]): R2

}
