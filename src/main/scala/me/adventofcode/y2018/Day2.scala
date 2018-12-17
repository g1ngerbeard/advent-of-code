package me.adventofcode.y2018

object Day2 {

  def count(ids: List[String]):(Int, Int) = {
    ids.map(single).reduce[(Int, Int)] { case ((a,b), (c,d)) => (a + c, b + d) }
  }

  def single(id: String): (Int, Int) = {
    val occurrences = id.groupBy(identity).mapValues(_.length)

    val pair = if (occurrences.values.exists(_ == 2)) 1 else 0
    val triple = if (occurrences.values.exists(_ == 3)) 1 else 0

    (pair, triple)
  }

  def findCommon(ids: List[String]): Option[String] =
    ids
      .combinations(2)
      .collectFirst { case List(a, b) if isSimilar(a, b) => commonPart(a, b) }

  def commonPart(a: String, b: String): String =
    ("" /: (a zip b)) {
      case (res, (ca, cb)) => if (ca != cb) res else res + ca
    }

  def isSimilar(a: String, b: String): Boolean = {
    val diffChars = (0 /: (a zip b)) {
      case (diff, (ca, cb)) => if (ca != cb) diff + 1 else diff
    }

    diffChars < 2
  }

  (0, 1)

}
