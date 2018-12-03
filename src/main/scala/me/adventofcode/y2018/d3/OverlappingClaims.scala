package me.adventofcode.y2018.d3

import me.adventofcode.y2018.util.Parsers._

import scala.util.matching.Regex

object OverlappingClaims {

  def count(input: Vector[Claim]): Int = input.foldLeft(FabricField.empty)(_ + _).conflicts


}

class FabricField(width: Int, height: Int, grid: Map[(Int, Int), Int] ) {

  lazy val conflicts: Int = grid.count{ case (_, claims) => claims > 1 }

  def +(claim: Claim): FabricField = {
    val Claim(_, leftMargin, topMargin, claimWidth, claimHeight) = claim

    val claimX = leftMargin + claimWidth - 1
    val claimY = topMargin + claimHeight - 1

    val claimGrid = for {
      i <- leftMargin to claimX
      j <- topMargin to claimY
    } yield (i, j)

    new FabricField(
      Math.max(width, claimX),
      Math.max(height, claimY),
      claimGrid.foldLeft(grid)((res, pos) => res + (pos -> (res.getOrElse(pos, 0) + 1)))
    )
  }

}

object FabricField {
  val empty: FabricField = new FabricField(0, 0, Map.empty)
}

case class Claim(id: Int, leftMargin: Int, topMargin: Int, width: Int, height: Int)

object Claim {

  private val claimRegex: Regex = "#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)".r

  //#1 @ 1,3: 4x4
  def parse(serialized: String): Either[String, Claim] = serialized match {
      case claimRegex(idStr, leftMarginStr, topMarginStr, widthStr, heightStr) =>
        for {
          id         <- idStr.parseInt
          leftMargin <- leftMarginStr.parseInt
          topMargin  <- topMarginStr.parseInt
          width      <- widthStr.parseInt
          height     <- heightStr.parseInt
        } yield Claim(id, leftMargin, topMargin, width,height)

      case _ => Left("Input do not match regex")
    }


}
