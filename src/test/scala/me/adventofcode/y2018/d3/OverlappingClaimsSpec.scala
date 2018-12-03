package me.adventofcode.y2018.d3

import me.adventofcode.y2018.d3.OverlappingClaimsSpec.{ParsedTestInput, TestInput}
import org.scalatest.{FunSuite, Matchers}

class OverlappingClaimsSpec extends FunSuite with Matchers {

  test("count overlapping square inches of fabric") {
    OverlappingClaims.count(ParsedTestInput) shouldBe 4
  }

  test("parse claims list") {
    val parsedInput = TestInput
      .split('\n')
      .flatMap(Claim.parse(_).toOption)
      .toVector

    parsedInput shouldEqual ParsedTestInput
  }

}

object OverlappingClaimsSpec {

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
