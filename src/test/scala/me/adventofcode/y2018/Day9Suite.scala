package me.adventofcode.y2018

import org.scalatest.{FunSuite, Matchers}

class Day9Suite extends FunSuite with Matchers {

  test("solve problem part 1") {
    Day9.highScore(10, 1618) shouldBe 8317
    Day9.highScore(13, 7999) shouldBe 146373
    Day9.highScore(17, 1104) shouldBe 2764
    Day9.highScore(21, 6111) shouldBe 54718
    Day9.highScore(30, 5807) shouldBe 37305
  }

}
