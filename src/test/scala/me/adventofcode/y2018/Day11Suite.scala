package me.adventofcode.y2018

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should

class Day11Suite extends AnyFunSuite with should.Matchers {

  val serial = 3214

  test("calculate power levels") {
    Day11.powerLevel(122, 79, 57) shouldBe -5
    Day11.powerLevel(217, 196, 39) shouldBe 0
    Day11.powerLevel(3, 5, 8) shouldBe 4
    Day11.powerLevel(101, 153, 71) shouldBe 4
  }

  test("find the most powerful region") {
    Day11.maxPower(18) shouldBe((33, 45), 29)
    Day11.maxPower(42) shouldBe((21, 61), 30)
    Day11.maxPower(3214) shouldBe((21, 42), 32)
  }

}
