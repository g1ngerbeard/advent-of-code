package me.adventofcode

import me.adventofcode.util.files

class TaskRunner[R1, R2](task: DayTask[R1, R2]) {

  private val input: Seq[String] = files.withResourceUnsafe(s"${task.year}/day${task.day}.txt")(identity)

  def solvePart1: R1 = task.part1(input)

  def solvePart2: R2 = task.part2(input)

}
