package me.adventofcode.y2018.d3

import me.adventofcode.y2018.d3.OverlappingClaimsSuite.{ParsedTestInput, TestInput, ProblemInput}
import org.scalatest.{FunSuite, Matchers}

import scala.io.Source

class OverlappingClaimsSuite extends FunSuite with Matchers {

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

object OverlappingClaimsSuite {

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
