package me.adventofcode.y2018.d2

import me.adventofcode.y2018.d2.InventoryManagementSystemSuite.ProblemInput
import org.scalatest.{FunSuite, Matchers}

import scala.io.Source

class InventoryManagementSystemSuite extends FunSuite with Matchers {

  test("count pairs and triples") {

    val inputIds = List(
      "abcdef",
      "bababc",
      "abbcde",
      "abcccd",
      "aabcdd",
      "abcdee",
      "ababab"
    )

    InventoryManagementSystem.count(inputIds) shouldBe (4, 3)
  }

  test("solve day 2 problem"){
    InventoryManagementSystem.count(ProblemInput) shouldBe (247,22)
  }

  test("solve day 2 problem - part 2"){
    println(InventoryManagementSystem.findCommon(ProblemInput))
  }

}

object InventoryManagementSystemSuite {

  lazy val ProblemInput: List[String] = Source
    .fromResource("2018/d2/box_ids.txt")
    .getLines()
    .toList


}
