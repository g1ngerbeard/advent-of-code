package me.adventofcode.y2018

import me.adventofcode.y2018.util.extensions._

import scala.annotation.tailrec

object Day9 extends App {

  def highScore(playersN: Int, lastMarble: Int): Int = {

    @tailrec
    def gameLoop(marbleRing: Ring, playerRing: Ring, playerScores: Map[Int, Int], marblePot: Vector[Int]): Int = {

      val currentPlayer = playerRing.current()
      val nextPlayerRing = playerRing >> 1

      marblePot match {
        case nextMarble +: rest =>
          if (nextMarble % 23 == 0) {
            val nextMarbleRing = marbleRing << 7
            val removedMarble = nextMarbleRing.current()
            val nextScores = playerScores.computeWithDefault(currentPlayer, _ + nextMarble + removedMarble, 0)

            gameLoop(
              (nextMarbleRing >> 1).dropLeft,
              nextPlayerRing,
              nextScores,
              rest
            )
          } else {
            gameLoop(
              marbleRing >> 1 >+ nextMarble >> 1,
              nextPlayerRing,
              playerScores,
              rest
            )
          }

        case _ => playerScores.values.max
      }
    }

    val playerIds = (0 until playersN).toVector

    gameLoop(
      Ring.from(0),
      Ring(playerIds, 0),
      playerIds.map(_ -> 0).toMap,
      (1 to lastMarble).toVector
    )
  }

}

case class GameState(marbleRing: Ring, playerRing: Ring, playerScores: Map[Int, Int], marblePot: Vector[Int])

object Ring {

  def from(marbleId: Int): Ring = new Ring(Vector(marbleId), 0)

}

case class Ring(marbles: Vector[Int], currentIndex: Int) {

  def current(): Int = marbles(currentIndex)

  def <<(positions: Int): Ring = Ring(marbles, shiftLeft(positions))

  def >>(positions: Int): Ring = Ring(marbles, shiftRight(positions))

  def <+(marbleId: Int): Ring =
    if (currentIndex == 0) {
      Ring(marbles :+ marbleId, currentIndex)
    } else {
      add(marbleId, currentIndex)
    }

  def >+(marbleId: Int): Ring =
    if (currentIndex == marbles.length - 1) {
      Ring(marbles :+ marbleId, currentIndex)
    } else {
      add(marbleId, shiftRight(1))
    }

  def dropRight: Ring =
    if (currentIndex == marbles.length - 1) {
      remove(0)
    } else {
      remove(shiftRight(1))
    }


  def dropLeft: Ring =
    if (currentIndex == 0) {
      remove(marbles.length - 1)
    } else {
      remove(shiftLeft(1))
    }

  def remove(position: Int): Ring = {
    val newElems = marbles
      .splitAt(position)
      .rightMap(_.drop(1))
      .fold(_ ++ _)

    val newIndex = if (position <= currentIndex) {
      currentIndex - 1
    } else {
      currentIndex
    }

    Ring(newElems, newIndex)
  }

  def add(newElem: Int, position: Int): Ring = {
    val newElems = marbles
      .splitAt(position)
      .leftMap(_ :+ newElem)
      .fold(_ ++ _)

    val newIndex = if (position <= currentIndex) {
      currentIndex + 1
    } else {
      currentIndex
    }

    Ring(newElems, newIndex)
  }

  private def shiftLeft(positions: Int): Int = {
    val newPos = currentIndex - (positions % marbles.length)
    if (newPos < 0) marbles.length + newPos else newPos
  }

  private def shiftRight(positions: Int): Int = (currentIndex + positions) % marbles.length

}


