package me.adventofcode.y2018

import me.adventofcode.y2018.Day1Suite.TestInput
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class Day1Suite extends AnyFunSuite with Matchers {

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
