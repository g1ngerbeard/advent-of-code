package me.adventofcode

import me.adventofcode.util.files
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should

abstract class DaySuite[R1, R2](dayTask: DayTask[R1, R2]) extends AnyFunSuite with should.Matchers {

  override def suiteName: String = s"AoC ${dayTask.year} Day ${dayTask.day} ${dayTask.name}"

  val fullInput: Seq[String] = files.withResourceUnsafe(s"${dayTask.year}/day${dayTask.day}.txt")(identity)

  def reducedInput: Seq[String]

  def testPartOneReduced(expected: R1): Unit =
    test("solve reduced part 1") {
      dayTask.part1(reducedInput) shouldBe expected
    }

  def testPartTwoReduced(expected: R2): Unit =
    test("solve reduced part 2") {
      dayTask.part2(reducedInput) shouldBe expected
    }

  def testPartOne(expected: R1): Unit =
    test("solve part 1") {
      dayTask.part1(fullInput) shouldBe expected
    }

  def testPartTwo(expected: R2): Unit =
    test("solve part 2") {
      dayTask.part2(fullInput) shouldBe expected
    }

}
