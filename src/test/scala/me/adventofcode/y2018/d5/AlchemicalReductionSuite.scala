package me.adventofcode.y2018.d5

import me.adventofcode.y2018.d5.AlchemicalReductionSuite.ProblemInput
import org.scalatest.{FunSuite, Inside, Matchers}

import scala.io.Source

class AlchemicalReductionSuite extends FunSuite with Matchers with Inside {

  test("reduce input polymer"){
    AlchemicalReduction.reduce("dabAcCaCBAcCcaDA") shouldBe "dabCBAcaDA"
  }

  test("solve day 5 problem part 1") {
    AlchemicalReduction.reduce(ProblemInput).length shouldBe 10762
  }

  ignore("solve day 5 problem part 2") {
    AlchemicalReduction.shortestReduction(ProblemInput).length shouldBe 6946
  }

}

object AlchemicalReductionSuite {
  val ProblemInput: String = Source
    .fromResource("2018/d5/polymer.txt")
    .getLines()
    .mkString
}
