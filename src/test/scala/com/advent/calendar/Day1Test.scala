package com.advent.calendar 

import com.advent.UnitTest

import scala.io.Source
import java.io.File
import com.advent.UnitTest
import com.advent.Universe

class Day1Test extends UnitTest {

  test("Fuel Counter-Upper") {
    val filePath = getClass.getClassLoader.getResource("fixtures/day1.txt").getPath

    val sumOfFuel = Source.fromFile(filePath).getLines.map(_.toDouble).map(Universe.fuelToLaunch).reduce(_ + _)
    println(sumOfFuel)
  }
}
