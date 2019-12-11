package me.adventofcode.y2019

import me.adventofcode.util.files
import me.adventofcode.y2019.Day1.{fuelRequirement, fuelRequirementPlus}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should

class Day1Suite extends AnyFunSuite with should.Matchers {

  test("solve part 1") {
    val totalSum = Day1Suite.PuzzleInput.map(fuelRequirement).sum
    totalSum shouldBe 3372463
  }

  test("calculate module fuel requirement plus") {
    fuelRequirementPlus(14) shouldBe 2
    fuelRequirementPlus(1969) shouldBe 966
    fuelRequirementPlus(100756) shouldBe 50346
  }

  test("solve part 2") {
    val totalSum = Day1Suite.PuzzleInput.map(fuelRequirementPlus).sum
    totalSum shouldBe 5055835
  }

}

object Day1Suite {

  val PuzzleInput: List[Int] = files.withResourceUnsafe("2019/day1.txt")(_.map(_.toInt).toList)

}
