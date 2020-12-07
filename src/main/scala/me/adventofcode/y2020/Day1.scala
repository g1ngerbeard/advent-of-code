package me.adventofcode.y2020

object Day1 {

  val checkValue = 2020

  def part1(entries: Vector[Int]): Int = {
    val products = for {
      i <- (0 until (entries.size - 1)).to(LazyList)
      a = entries(i)
      j <- ((i + 1) until entries.size).to(LazyList)
      b = entries(j)
      if a + b == checkValue
    } yield a * b

    products.headOption.getOrElse(-1)
  }

  def part2(entries: Vector[Int]): Int = {
    val products = for {
      i <- (0 until (entries.size - 2)).to(LazyList)
      a = entries(i)
      j <- ((i + 1) until entries.size - 1).to(LazyList)
      b = entries(j)
      k <- ((j + 1) until entries.size).to(LazyList)
      c = entries(k)
      if a + b + c == checkValue
    } yield a * b * c

    products.headOption.getOrElse(-1)
  }

}
