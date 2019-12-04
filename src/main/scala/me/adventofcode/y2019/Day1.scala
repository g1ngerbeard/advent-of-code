package me.adventofcode.y2019

object Day1 {

  def fuelRequirement(mass: Int): Int = math.max(0, mass / 3 - 2)

}
