package me.adventofcode.y2018

import me.adventofcode.y2018.Day3Spec.{ParsedTestInput, ProblemInput, TestInput}

import scala.io.Source
import org.scalatest.{FunSuite, Inside, Matchers}

import scala.collection.JavaConverters._

class Day3Spec extends FunSuite with Matchers {

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

object Day3Spec {

  lazy val ProblemInput: Vector[Claim] = Source
    .fromResource("2018/d3/claims.txt")
    .getLines()
    .flatMap(Claim.parse(_).toOption)
    .toVector

  val ParsedTestInput = Vector(
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
