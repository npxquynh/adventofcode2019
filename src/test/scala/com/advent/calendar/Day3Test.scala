package com.advent.calendar

import com.advent.UnitTest

import scala.io.Source
import java.io.File
import com.advent.UnitTest
import com.advent.Universe
import com.advent.CrossWires

class Day3Test extends UnitTest {

  test("Manhattan distance for 2 cross wires") {
    val filePath = getClass.getClassLoader.getResource("fixtures/day3.txt").getPath
    val input = Source.fromFile(filePath).getLines.map(_.split(",")).toArray

    val minManhattanDistanceForCrossedPoint = CrossWires.execute(input(0), input(1))
    println(s"===> Day 3a: ${minManhattanDistanceForCrossedPoint}") // 217
  }
}
