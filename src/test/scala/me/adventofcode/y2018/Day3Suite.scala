package me.adventofcode.y2018

import me.adventofcode.y2018.Day3Suite.{ParsedTestInput, ProblemInput, TestInput}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should

import scala.io.Source

class Day3Suite extends AnyFunSuite with should.Matchers {

  test("count overlapping square inches of fabric") {
    FabricField.from(ParsedTestInput).conflicts shouldBe 4
  }

  test("parse claims list") {
    val parsedInput = TestInput
      .split('\n')
      .flatMap(Claim.parse(_).toOption)
      .toVector

    parsedInput shouldEqual ParsedTestInput
  }

  test("solve day 3 task") {
    FabricField.from(ProblemInput).conflicts shouldEqual 110546
  }

  test("find non overlapping claims") {
    FabricField.from(ProblemInput).nonOverlappingIds shouldBe Set(819)
  }

}

object Day3Suite {

  lazy val ProblemInput: Vector[Claim] = Source
    .fromResource("2018/day3.txt")
    .getLines()
    .flatMap(Claim.parse(_).toOption)
    .toVector

  val ParsedTestInput: Vector[Claim] = Vector(
    Claim(1, 1, 3, 4, 4),
    Claim(2, 3, 1, 4, 4),
    Claim(3, 5, 5, 2, 2)
  )

  val TestInput: String =
    """
      |#1 @ 1,3: 4x4
      |#2 @ 3,1: 4x4
      |#3 @ 5,5: 2x2
    """.stripMargin

}
