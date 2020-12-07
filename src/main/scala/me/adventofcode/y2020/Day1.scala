package me.adventofcode.y2020

object Day1 {

  val checkValue = 2020

  def part1(entries: Vector[Int]): Int = {
    val products = for {
      i <- LazyList.range(0, entries.size - 2)
      j <- LazyList.range(i + 1, entries.size - 1)
      values = Vector(i, j).map(entries)
      if values.sum == checkValue
    } yield values.product

    products.headOption.getOrElse(-1)
  }

  def part2(entries: Vector[Int]): Int = {
    val products = for {
      i <- LazyList.range(0, entries.size - 2)
      j <- LazyList.range(i + 1, entries.size - 1)
      k <- LazyList.range(j + 1, entries.size)
      values = Vector(i, j, k).map(entries)
      if values.sum == checkValue
    } yield values.product

    products.headOption.getOrElse(-1)
  }

}
