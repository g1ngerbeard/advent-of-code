package me.adventofcode.util

import scala.io.Source

object files {

  def withResourceUnsafe[T](name: String)(f: List[String] => T): T = {
    val source = Source.fromResource(name)

    try {
      if (source == null) throw new Exception(s"Could not open resource $name")
      val result = f(source.getLines().toList)
      result
    } finally {
      source.close()
    }
  }

}
