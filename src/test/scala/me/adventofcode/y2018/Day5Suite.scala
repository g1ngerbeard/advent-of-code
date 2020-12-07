package me.adventofcode.y2018

import me.adventofcode.y2018.Day5Suite.ProblemInput
import org.scalatest.Inside
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should

import scala.io.Source

class Day5Suite extends AnyFunSuite with should.Matchers with Inside {

  test("reduce input polymer"){
    Day5.reduce("dabAcCaCBAcCcaDA") shouldBe "dabCBAcaDA"
  }

  test("solve day 5 problem part 1") {
    Day5.reduce(ProblemInput).length shouldBe 10762
  }

  ignore("solve day 5 problem part 2") {
    Day5.shortestReduction(ProblemInput).length shouldBe 6946
  }

}

object Day5Suite {
  val ProblemInput: String = Source
    .fromResource("2018/day5.txt")
    .getLines()
    .mkString
}
