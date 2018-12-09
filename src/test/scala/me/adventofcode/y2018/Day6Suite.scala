package me.adventofcode.y2018

import me.adventofcode.y2018.Day6Suite.ProblemInput
import me.adventofcode.y2018.util.Parsers._
import org.scalatest.{FunSuite, Matchers}

import scala.io.Source

class Day6Suite extends FunSuite with Matchers {

  test("solve problem part 1") {
    Day6.largestFiniteArea(ProblemInput) shouldBe 4186
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


  val TestInput: Vector[(Int, Int)] = Vector(
    1 -> 1,
    1 -> 6,
    8 -> 3,
    3 -> 4,
    5 -> 5,
    8 -> 9
  )

}