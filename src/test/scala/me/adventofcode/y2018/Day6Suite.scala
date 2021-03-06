package me.adventofcode.y2018

import me.adventofcode.util.parsers._
import me.adventofcode.y2018.Day6Suite.ProblemInput
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should

import scala.io.Source

class Day6Suite extends AnyFunSuite with should.Matchers {

  test("solve problem part 1") {
    Day6.largestFiniteArea(ProblemInput) shouldBe 4186
  }

  test("solve problem part 2"){
    println(Day6.adjacentAreaSize(ProblemInput))
  }

}

object Day6Suite {

  val ProblemInput: Vector[(Int, Int)] = Source
    .fromResource("2018/day6.txt")
    .getLines()
    .toVector
    .flatMap(_.split(", ") match {
      case Array(xStr, yStr) =>
        for {
           x <- xStr.parseInt.toOption
           y <- yStr.parseInt.toOption
        } yield (x, y)
        case _ => None
    })

}
