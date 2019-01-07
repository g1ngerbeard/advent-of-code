package me.adventofcode.y2018

import me.adventofcode.y2018.util.extensions._

object Day11 extends App {

  type Coordinate = (Int, Int)

  def powerLevel(x: Int, y: Int, gridSerial: Int): Int = {
    val rackId = x + 10
    ((rackId * y + gridSerial) * rackId) % 1000 / 100 - 5
  }

  def totalPower(x: Int, y: Int, grid: Map[Coordinate, Int]): Int = {
    val region3x3 = for {
      i <- x to x + 2
      j <- y to y + 2
    } yield grid(i -> j)

    region3x3.sum
  }

  def maxPower(serial: Int): (Coordinate, Int) = {
    val grid = (for {
      x <- 0 until 300
      y <- 0 until 300
    } yield (x -> y, powerLevel(x, y, serial))).toMap

    val regions = for {
      x <- 0 until 297
      y <- 0 until 297
    } yield (x -> y, totalPower(x, y, grid))

    regions.maxBy(_.right)
  }

}
