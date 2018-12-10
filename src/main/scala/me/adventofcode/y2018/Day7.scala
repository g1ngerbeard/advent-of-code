package me.adventofcode.y2018


import scala.annotation.tailrec
import scala.util.matching.Regex

object Day7 {

  def order(instructions: Vector[(String, String)]): String =
    Graph.from(instructions).traverseOrder().mkString

}

object Graph {

  val empty = new Graph(Set.empty, Map.empty)

  def from(steps: Vector[(String, String)]): Graph = (empty /: steps)(_ + _)

}

class Graph private (val steps: Set[String], val requirementsByStep: Map[String, Set[String]]) {

  def +(requirement: (String, String)): Graph = {
    val (requiredStep, step) = requirement

    val requirements = requirementsByStep.getOrElse(step, Set.empty)

    new Graph(
      steps + requiredStep + step,
      requirementsByStep + (step -> (requirements + requiredStep))
    )
  }

   def traverseOrder(): Vector[String] = {
     
    @tailrec
    def loop(done: Vector[String], pending: Vector[String]): Vector[String] = {

      val next = pending.find { step =>
        val requirements = requirementsByStep.getOrElse(step, Set.empty)
        requirements.isEmpty || requirements.forall(done.contains)
      }

      next match {
        case Some(step) => loop(done :+ step, pending.filterNot(_ == step))
        case None => done
      }
    }

    loop(Vector.empty, steps.toVector.sorted)
  }

}

object Instruction {

  val InstructionRegex: Regex = "Step (\\w) must be finished before step (\\w) can begin.".r

  def parse(input: String): Option[(String, String)] = input match {
    case InstructionRegex(from, to) => Some(from -> to)
    case _ => None
  }
}