package com.advent

import scala.math

object Universe {

  /** 
   * Fuel required to launch a given module is based on its mass. Specifically, to find the fuel required for a 
   * module, take its mass, divide by three, round down, and subtract 2.
   * 
   * Also need to take into account that fuel has weight, and to lift that fuel up, we will need more fuel.
   */
  def fuelToLaunch(mass: Double): Double = {
    val fuelEquation: Double => Double = m => math.floor(m / 3.0).toInt - 2.0

    def helper(mass: Double, acc: List[Double]): List[Double] = {
      val fuelNeeded = fuelEquation(mass)
      if (fuelNeeded <= 0) return acc
      else helper(fuelNeeded, fuelNeeded :: acc)
    }

    helper(mass, List.empty[Double]).reduce(_ + _)
  }
}