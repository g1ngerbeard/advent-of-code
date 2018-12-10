package me.adventofcode.y2018

import me.adventofcode.y2018.Day1Suite.TestInput

import scala.io.Source
import org.scalatest.{FunSuite, Inside, Matchers}

class Day1Suite extends FunSuite with Matchers {

  test("solve day 1 problem") {
    Day1.resultingFrequency(TestInput) shouldEqual 531
  }

  ignore("solve day 1 problem - part 2") {
    Day1.firstRepeatedFrequency(TestInput)
  }

}

object Day1Suite {

  val TestInput: Vector[Int] = Source
    .fromResource("2018/day1.txt")
    .getLines()
    .toVector
    .flatMap(Day1.parseChange)

}
