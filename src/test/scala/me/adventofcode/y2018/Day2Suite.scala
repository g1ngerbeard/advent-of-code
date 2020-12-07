package me.adventofcode.y2018

import me.adventofcode.y2018.Day2Suite.ProblemInput
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should

import scala.io.Source

class Day2Suite extends AnyFunSuite with should.Matchers {

  test("count pairs and triples") {

    val inputIds = List(
      "abcdef",
      "bababc",
      "abbcde",
      "abcccd",
      "aabcdd",
      "abcdee",
      "ababab"
    )

    Day2.count(inputIds) shouldBe (4, 3)
  }

  test("solve day 2 problem"){
    Day2.count(ProblemInput) shouldBe (247,22)
  }

  test("solve day 2 problem - part 2"){
    Day2.findCommon(ProblemInput) shouldBe Some("agimdjvlhedpsyoqfzuknpjwt")
  }

}

object Day2Suite {

  lazy val ProblemInput: List[String] = Source
    .fromResource("2018/day2.txt")
    .getLines()
    .toList

}
