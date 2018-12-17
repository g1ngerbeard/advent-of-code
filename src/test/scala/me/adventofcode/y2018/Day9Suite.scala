package me.adventofcode.y2018

import org.scalatest.{FunSuite, Matchers}

class Day9Suite extends FunSuite with Matchers {

  ignore("solve problem part 1") {
    Day9.highScore(10, 1618) shouldBe 8317
    Day9.highScore(13, 7999) shouldBe 146373
    Day9.highScore(17, 1104) shouldBe 2764
    Day9.highScore(21, 6111) shouldBe 54718
    Day9.highScore(30, 5807) shouldBe 37305
  }

  test("add and remove elements to ring") {

    Ring(Vector(1, 2), 0) <+ 3 shouldBe Ring(Vector(3, 1, 2), 1)

    Ring(Vector(1, 2, 3), 2) >+ 4 shouldBe Ring(Vector(4, 1, 2, 3), 3)

    Ring(Vector(1, 2, 3), 2).dropLeft shouldBe Ring(Vector(1, 3), 1)

    Ring(Vector(1, 2, 3), 0).dropLeft shouldBe Ring(Vector(1, 2), 0)

    Ring(Vector(1, 2, 3), 2).dropRight shouldBe Ring(Vector(2, 3), 1)

    Ring(Vector(1, 2, 3), 0).dropRight shouldBe Ring(Vector(1, 3), 0)

    val ring = Ring(Vector(1, 2, 3, 4), 0)

    ring >+ 5 >+ 6 shouldBe Ring(Vector(1, 6, 5, 2, 3, 4), 0)

    ring >> 2 <+ 5 <+ 6 shouldBe Ring(Vector(1, 2, 5, 6, 3, 4), 4)

    ring.dropLeft >> 2 >+ 5 shouldBe Ring(Vector(1, 2, 3, 5), 3)

  }

}
