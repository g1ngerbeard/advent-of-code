package me.adventofcode.y2018

import me.adventofcode.y2018.util.extensions._

object Day9 extends App {

  def highScore(playersN: Int, lastMarble: Int): Int = {

    val playerIds = (0 until playersN).toVector
    val playersScores = playerIds.map(_ -> 0)

    val playerRing = Ring(playerIds, 0)


    //    MarbleRing.from()


    0
  }

}


object Ring {

  def from(marbleId: Int): Ring = new Ring(Vector(marbleId), 0)

}

case class Ring(elems: Vector[Int], currentIndex: Int) {

  def <<(positions: Int): Ring = Ring(elems, shiftLeft(positions))

  def >>(positions: Int): Ring = Ring(elems, shiftRight(positions))

  def >+(marbleId: Int): Ring = add(marbleId, shiftRight(1))

  def <+(marbleId: Int): Ring = add(marbleId, shiftLeft(1))

  def dropRight : Ring = remove(shiftRight(1))

  def dropLeft : Ring = remove(shiftLeft(1))

  def remove(position: Int): Ring = {
    val newElems = elems
      .splitAt(position)
      .rightMap(_.drop(1))
      .fold(_ ++ _)

    val newIndex = if (position < currentIndex) {
      currentIndex - 1
    } else {
      currentIndex
    }

    Ring(newElems, newIndex)
  }

  def add(newElem: Int, position: Int): Ring = {
    val newElems = elems
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
    val newPos = currentIndex - (positions / elems.length)
    if (newPos < 0) elems.length + newPos else newPos
  }

  private def shiftRight(positions: Int): Int = (currentIndex + positions) % elems.length

}


