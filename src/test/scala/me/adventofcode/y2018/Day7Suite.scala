package me.adventofcode.y2018

import me.adventofcode.y2018.Day7Suite.ProblemInput
import me.adventofcode.y2018.util.files.withResourceUnsafe
import org.scalatest.{FunSuite, Matchers}

class Day7Suite extends FunSuite with Matchers {

  test("solve problem part 1") {
    println(Day7.order(ProblemInput))
  }

}

object Day7Suite {

  val ProblemInput: Vector[(String, String)] = withResourceUnsafe("2018/day8.txt") {
      _.toVector.flatMap(Instruction.parse)
    }

}
