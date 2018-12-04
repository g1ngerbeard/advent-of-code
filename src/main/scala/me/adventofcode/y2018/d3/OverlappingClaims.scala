package me.adventofcode.y2018.d3

import me.adventofcode.y2018.util.Parsers._

import scala.util.matching.Regex

class FabricField(grid: Map[(Int, Int), Set[Int]] ) {

  lazy val nonOverlappingIds: Set[Int] = {
    val (conflictClaims, nonConflictCandidates) = grid.values.toSet.partition(_.size > 1)
    nonConflictCandidates.flatten &~ conflictClaims.flatten
  }

  lazy val conflicts: Int = grid.count { case (_, claims) => claims.size > 1 }

  def add(claim: Claim): FabricField = {
    val Claim(claimId, leftMargin, topMargin, claimWidth, claimHeight) = claim

    val claimX = leftMargin + claimWidth - 1
    val claimY = topMargin + claimHeight - 1

    val claimGrid = for {
      i <- leftMargin to claimX
      j <- topMargin to claimY
    } yield (i, j)

    new FabricField(
      claimGrid.foldLeft(grid)((res, pos) => res + (pos -> (res.getOrElse(pos, Set.empty) + claimId)))
    )
  }

}

object FabricField {
  def from(input: Vector[Claim]):FabricField = input.foldLeft(FabricField.empty)(_ add _)

  val empty: FabricField = new FabricField(Map.empty)
}

case class Claim(id: Int, leftMargin: Int, topMargin: Int, width: Int, height: Int)

object Claim {

  private val claimRegex: Regex = "#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)".r

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
