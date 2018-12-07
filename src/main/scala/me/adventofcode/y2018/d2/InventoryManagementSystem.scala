package me.adventofcode.y2018.d2

object InventoryManagementSystem {

  def count(ids: List[String]):(Int, Int) = {
    ids.map(single).reduce[(Int, Int)] { case ((a,b), (c,d)) => (a + c, b + d) }
  }

  def single(id: String): (Int, Int) = {
    val occurances = id.groupBy(identity).mapValues(_.length)

    val pair = if (occurances.values.exists(_ == 2)) 1 else 0
    val triple = if (occurances.values.exists(_ == 3)) 1 else 0

    (pair, triple)
  }

}
