package me.adventofcode.y2018

object Day8 {

  def buildGraph(input: Vector[Int]): Node = Node.parse(input)._1

  def sum(node: Node): Int = node.metadata.sum + node.children.map(sum).sum

  def value(node: Node): Int = if (node.children.nonEmpty) {
    node
      .metadata
      .map(_ - 1)
      .map(node.children.lift)
      .flatten
      .map(value)
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
      case ((childrenRes, tailRes) ,_) =>
        val (node, newTail) = parse(tailRes)
        (childrenRes :+ node, newTail)
    }

    val (metadata, restWOMetadata) = restWOChildren.splitAt(metadataSize)

    (Node(metadata, children), restWOMetadata)
  }

}


case class Node(metadata: Vector[Int], children: Vector[Node])
