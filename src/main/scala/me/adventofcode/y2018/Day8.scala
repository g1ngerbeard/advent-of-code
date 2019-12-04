package me.adventofcode.y2018

import me.adventofcode.util.extensions._

object Day8 {

  def buildGraph(input: Vector[Int]): Node = Node.parse(input).left

  def sum(node: Node): Int = node.metadata.sum + node.children.map(sum).sum

  def value(node: Node): Int = node match {
    case Node(metadata, children) if children.nonEmpty =>
      metadata
        .flatMap(i => children.lift(i - 1).map(value))
        .sum

    case Node(metadata, _) => metadata.sum
  }
}

object Node {

  def parse(input: Vector[Int]): (Node, Vector[Int]) = {
    val (Vector(childrenSize, metadataSize), rest) = input.splitAt(2)

    val (children, restWOChildren) = ((Vector.empty[Node], rest) /: (0 until childrenSize)) {
      case ((childrenRes, tailRes), _) => parse(tailRes).leftMap(childrenRes :+ _)
    }

    restWOChildren
      .splitAt(metadataSize)
      .leftMap(Node(_, children))
  }

}

case class Node(metadata: Vector[Int], children: Vector[Node])
