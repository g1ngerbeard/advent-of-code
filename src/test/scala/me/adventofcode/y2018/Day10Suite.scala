package me.adventofcode.y2018

import me.adventofcode.util.files.withResourceUnsafe
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should

class Day10Suite extends AnyFunSuite with should.Matchers {

  ignore("solve problem part 1") {
    val input = withResourceUnsafe("2018/day10.txt")(_.flatMap(Point.parse).toVector)

    val coordinates = Day10.minPositions(input, 55000)
    println(Day10.render(coordinates))
  }

  val response = "ECKXJLJF"

}
