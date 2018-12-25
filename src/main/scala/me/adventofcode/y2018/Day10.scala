package me.adventofcode.y2018

import me.adventofcode.y2018.Point.Coordinate
import me.adventofcode.y2018.util.parsers._
import me.adventofcode.y2018.util.extensions._

import scala.util.matching.Regex

object Day10 {

  def minPositions(init: Vector[Point], iter: Int): Vector[Coordinate] =
    (0 to iter)
      .scanLeft(init)((pos, _) => pos.map(_.next()))
      .map(_.map(_.position))
      .minBy(square)

  private def square(positions: Vector[Coordinate]): Long = {
    val ((minX, minY), (maxX, maxY)) = boundaries(positions)
    (maxX - minX) * (maxY - minY)
  }

  def render(positions: Vector[Coordinate]): String = {
    val ((minX, minY), (maxX, maxY)) = boundaries(positions)

    val field = positions
        .groupBy(_.right)
        .mapValues(_.unzip.left.toSet)

    val rows = for {
      y <- minY to maxY
      rowXs = field.getOrElse(y, Set.empty)
    } yield (minX to maxX).map(mark(rowXs)).mkString

    rows.mkString("\n")
  }

  private def boundaries(positions: Vector[Coordinate]): (Coordinate, Coordinate) = {
    val (xs, ys) = positions.unzip
    ((xs.min, ys.min), (xs.max, ys.max))
  }

  private def mark(rowXs:Set[Long])(x: Long): String = if (rowXs.contains(x)) "#" else "."
}

case class Point(position: Coordinate, velocity: Coordinate) {

  def next(): Point = Point(position.map2(_ + velocity.left, _ + velocity.right), velocity)

}

object Point {

  type Coordinate = (Long, Long)

  val Regex: Regex = "position=<\\s*(-?\\d+), \\s*(-?\\d+)> velocity=<\\s?(-?\\d+), \\s?(-?\\d+)>".r

  def parse(value: String): Option[Point] = value match {
    case Regex(startXStr, startYStr, velocityXStr, velocityYStr) =>
      for {
        startX    <- startXStr.optInt
        startY    <- startYStr.optInt
        velocityX <- velocityXStr.optInt
        velocityY <- velocityYStr.optInt
      } yield Point((startX, startY), (velocityX, velocityY))
    case _       => None
  }

}
