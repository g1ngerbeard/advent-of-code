package me.adventofcode.y2020

import me.adventofcode.util.files
import me.adventofcode.y2020.Day2Suite.Part1Input
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should

class Day2Suite extends AnyFunSuite with should.Matchers {

  test("part 1") {
    Day2.part1(Part1Input) shouldBe 465
  }

  test("part 2") {
    Day2.part2(Part1Input) shouldBe 294
  }

}

object Day2Suite {

  val Part1Input: List[String] = files.withResourceUnsafe("2020/day2.txt")(identity)

}
