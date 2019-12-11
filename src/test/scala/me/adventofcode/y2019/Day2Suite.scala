package me.adventofcode.y2019

import me.adventofcode.y2019.Day2.runIntcode
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should

class Day2Suite extends AnyFunSuite with should.Matchers {

  test("run incode program") {
    runIntcode(Vector(1, 0, 0, 0, 99)) shouldBe Right(Vector(2, 0, 0, 0, 99))
    runIntcode(Vector(2, 3, 0, 3, 99)) shouldBe Right(Vector(2, 3, 0, 6, 99))
    runIntcode(Vector(2, 4, 4, 5, 99, 0)) shouldBe Right(Vector(2, 4, 4, 5, 99, 9801))
    runIntcode(Vector(1, 1, 1, 4, 99, 5, 6, 0, 99)) shouldBe Right(Vector(30, 1, 1, 4, 2, 5, 6, 0, 99))
  }

  test("solve day2 part1") {
    runIntcode(Day2Suite.PartOneInput) should matchPattern {
      case Right(2890696 +: _) =>
    }
  }

}

object Day2Suite {

  val Input: Vector[Int] = Vector(
    1, 0, 0, 3, 1, 1, 2, 3, 1, 3, 4, 3, 1, 5, 0, 3, 2, 10, 1, 19, 2, 9,
    19, 23, 2, 13, 23, 27, 1, 6, 27, 31, 2, 6, 31, 35, 2, 13, 35, 39,
    1, 39, 10, 43, 2, 43, 13, 47, 1, 9, 47, 51, 1, 51, 13, 55, 1, 55, 13,
    59, 2, 59, 13, 63, 1, 63, 6, 67, 2, 6, 67, 71, 1, 5, 71, 75, 2, 6,
    75, 79, 1, 5, 79, 83, 2, 83, 6, 87, 1, 5, 87, 91, 1, 6, 91, 95, 2,
    95, 6, 99, 1, 5, 99, 103, 1, 6, 103, 107, 1, 107, 2, 111, 1, 111, 5,
    0, 99, 2, 14, 0, 0
  )

  val PartOneInput: Vector[Int] = Input.updated(1, 12).updated(2, 2)

}