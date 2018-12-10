package me.adventofcode.y2018

object Day6 {

  type Coordinate = (Int, Int)

  def largestFiniteArea(coordinates: Vector[Coordinate]): Int = {

    def closestCoordinates(point: Coordinate): Vector[Coordinate] =
      coordinates
        .groupBy(distance(_, point))
        .minBy(_._1)
        ._2

    val (leftX, rightX, topY, bottomY) = areaBorders(coordinates)

    val areasCoverage = for {
      x <- leftX to rightX
      y <- topY to bottomY
    } yield (x, y)

    areasCoverage
      .groupBy(closestCoordinates)
      .filterKeys(_.size == 1)
      .values
      .filterNot(_.exists {
        case (x, y) => x == leftX || x == rightX || y == topY || y == bottomY
      })
      .map(_.size)
      .max
  }

  def adjacentAreaSize(coordinates: Vector[Coordinate]): Int = {
    val (leftX, rightX, topY, bottomY) = areaBorders(coordinates)

    val adjacentArea = for {
      x <- leftX to rightX
      y <- topY to bottomY
      totalDistance = coordinates.map(distance((x, y), _)).sum
      if totalDistance < 10000
    } yield (x,y)

    adjacentArea.size
  }

  private def areaBorders(coordinates: Vector[Coordinate]): (Int, Int, Int, Int) = {
    val (xs, ys) = coordinates.unzip
    (xs.min, xs.max, ys.min, ys.max)
  }

  private def distance(a: Coordinate, b: Coordinate): Int = Math.abs(a._1 - b._1) + Math.abs(a._2 - b._2)

}
