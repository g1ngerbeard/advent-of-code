package me.adventofcode.y2020

object Day1 {

  val checkValue = 2020

  def part1(entries: Vector[Int]): Int = {
    val products = for {
      i <- LazyList.range(0, entries.size - 2)
      a = entries(i)
      j <- LazyList.range(i + 1, entries.size - 1)
      b = entries(j)
      if a + b == checkValue
    } yield a * b

    products.headOption.getOrElse(-1)
  }

  def part2(entries: Vector[Int]): Int = {
    val products = for {
      i <- LazyList.range(0, entries.size - 2)
      a = entries(i)
      j <- LazyList.range(i + 1, entries.size - 1)
      b = entries(j)
      k <- LazyList.range(j + 1, entries.size)
      c = entries(k)
      if a + b + c == checkValue
    } yield a * b * c

    products.headOption.getOrElse(-1)
  }

}
