package me.adventofcode.y2019

import scala.annotation.tailrec

object Day1 {

  def fuelRequirement(mass: Int): Int = math.max(0, mass / 3 - 2)

  def fuelRequirementPlus(mass: Int): Int = {

    @tailrec
    def loop(sum: Int, currMass: Int): Int = {
      val fuelMass = fuelRequirement(currMass)

      if (fuelMass > 0) {
        loop(sum + fuelMass, fuelMass)
      } else {
        sum
      }
    }

    loop(0, mass)
  }


}
