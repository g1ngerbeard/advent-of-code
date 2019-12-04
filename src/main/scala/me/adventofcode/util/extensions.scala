package me.adventofcode.util

object extensions {

  implicit class Tuple2Extension[L, R](tuple2: (L, R)) {

    def map2[TL, TR](lf: L => TL, rf: R => TR): (TL, TR) = (lf(tuple2._1), rf(tuple2._2))

    def leftMap[T](f: L => T): (T, R) = map2(f, identity)

    def rightMap[T](f: R => T): (L, T) = map2(identity, f)

    def left: L = tuple2._1

    def right: R = tuple2._2

    def fold[T](f: (L, R) => T): T = f(tuple2._1, tuple2._2)

  }

  implicit class MapExtension[K, V](map: Map[K, V]) {

    def computeWithDefault(k: K, f: V => V, default: V): Map[K, V] = compute(k, _.map(f).orElse(Some(default)))

    def compute(k: K, f: Option[V] => Option[V]): Map[K, V] = f(map.get(k)).map(map.updated(k, _)).getOrElse(map)

    def valueOfMaxKey(implicit cmp: Ordering[K]): V = map.maxBy(_.left).right

    def keyOfMaxValue(implicit cmp: Ordering[V]): K = map.maxBy(_.right).left

  }

}
