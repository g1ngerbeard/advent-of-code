package me.adventofcode.y2018

import scala.util.Try

object Day1 {

  def resultingFrequency(input: Vector[Int]): Int = input.sum

  def firstRepeatedFrequency(input: Vector[Int]): Option[Int] =
    input
      .scanLeft(Vector(0)) { case (seen, change) =>
        seen :+ (seen.last + change)
      }
      .collectFirst {
        case seen :+ last if seen.contains(last) => last
      }

  def parseChange(change: String): Option[Int] = Try(change.toInt).toOption

}
