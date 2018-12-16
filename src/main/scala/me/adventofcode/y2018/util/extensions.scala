package me.adventofcode.y2018.util

object extensions {

  implicit class Tuple2Extension[L, R](tuple2: (L, R)) {

    def map2[TL, TR](lf: L => TL, rf: R => TR): (TL, TR) = (lf(tuple2._1), rf(tuple2._2))

    def leftMap[T](f: L => T): (T, R) = map2(f, identity)

    def rightMap[T](f: R => T): (L, T) = map2(identity, f)

    def left: L = tuple2._1

    def right: R = tuple2._2

  }

}
