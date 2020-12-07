package me.adventofcode.y2018

import me.adventofcode.util.files.withResourceUnsafe
import me.adventofcode.y2018.Day7Suite.ProblemInput
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should

class Day7Suite extends AnyFunSuite with should.Matchers {

  test("solve problem part 1") {
    println(Day7.order(ProblemInput))
  }

}

object Day7Suite {

  val ProblemInput: Vector[(String, String)] = withResourceUnsafe("2018/day8.txt") {
      _.toVector.flatMap(Instruction.parse)
    }

}
