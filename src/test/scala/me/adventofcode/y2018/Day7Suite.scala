package me.adventofcode.y2018

import me.adventofcode.y2018.Day7Suite.ProblemInput
import org.scalatest.{FunSuite, Matchers}

import scala.io.Source

class Day7Suite extends FunSuite with Matchers {

  test("solve problem part 1") {
    println(Day7.order(ProblemInput))
  }

}

object Day7Suite {

  val ProblemInput: Vector[(String, String)] = Source
    .fromResource("2018/day7.txt")
    .getLines()
    .toVector
    .flatMap(Instruction.parse)

}
