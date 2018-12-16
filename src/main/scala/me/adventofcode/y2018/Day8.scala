package me.adventofcode.y2018

import me.adventofcode.y2018.util.extensions._

object Day8 {

  def buildGraph(input: Vector[Int]): Node = Node.parse(input).left

  def sum(node: Node): Int = node.metadata.sum + node.children.map(sum).sum

  def value(node: Node): Int = if (node.children.nonEmpty) {
    val lifted = node.children.lift

    node
      .metadata
      .flatMap(i => lifted(i - 1).map(value))
      .sum
  } else {
    node.metadata.sum
  }
}

object Node {

  def parse(input: Vector[Int]): (Node, Vector[Int]) = {
    val (header, rest) = input.splitAt(2)

    val childrenSize = header(0)
    val metadataSize = header(1)

    val (children, restWOChildren) = ((Vector.empty[Node], rest) /: (0 until childrenSize)) {
      case ((childrenRes, tailRes), _) => parse(tailRes).leftMap(childrenRes :+ _)
    }

    restWOChildren
      .splitAt(metadataSize)
      .leftMap(Node(_, children))
  }

}


case class Node(metadata: Vector[Int], children: Vector[Node])
