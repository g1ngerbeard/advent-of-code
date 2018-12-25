package me.adventofcode.y2018.util

import scala.collection.Iterator
import scala.io.Source

object files {

  def withResourceUnsafe[T](name: String)(f: Iterator[String] => T): T = {
    val source = Source.fromResource(name)

    if (source == null) throw new Exception(s"Could not open resource $name")

    val result = f(source.getLines())
    source.close()
    result
  }

}
