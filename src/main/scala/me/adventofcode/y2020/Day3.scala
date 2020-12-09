package me.adventofcode.y2020

import me.adventofcode.DayTask

object Day3 extends DayTask[Int, Int](3, 2020, "Toboggan Trajectory") {

  val EmptyCell = '.'
  val TreeCell = '#'

  def part1(input: Seq[String]): Int = countTrees(input, 3, 1)

  def part2(input: Seq[String]): Int =
    Seq(
      1 -> 1,
      3 -> 1,
      5 -> 1,
      7 -> 1,
      1 -> 2
    ).map {
      case (xstep, ystep) => countTrees(input, xstep, ystep)
    }.product

  private def countTrees(input: Seq[String], xstep: Int, ystep: Int): Int = {
    val width = input.head.length

    LazyList
      .iterate((0, 0)) {
        case (x, y) => (x + xstep) -> (y + ystep)
      }
      .takeWhile(_._2 < input.size)
      .map {
        case (x, y) =>
          val xpos = x % width
          input(y)(xpos)
      }
      .count(_ == TreeCell)
  }

}
