package me.adventofcode.y2018

import me.adventofcode.y2018.Day8Suite.ProblemInput
import org.scalatest.{FunSuite, Matchers}

import scala.io.Source
import scala.util.Try

class Day8Suite extends FunSuite with Matchers {

  test("solve problem part 1") {

    println(Day8.sum(Day8.buildGraph(ProblemInput)))

  }

}

object Day8Suite {

  val ProblemInput: Vector[Int] = Source
    .fromResource("2018/day8.txt")
    .getLines()
    .mkString
    .split(" ")
    .flatMap(i => Try(i.toInt).toOption)
    .toVector

}
