package com.advent.calendar

import com.advent.UnitTest

import scala.io.Source
import java.io.File
import com.advent.CrossWires

class Day3Test extends UnitTest {

  test("Manhattan distance for 2 cross wires") {
    val filePath = getClass.getClassLoader.getResource("fixtures/day3.txt").getPath
    val input = Source.fromFile(filePath).getLines.map(_.split(",")).toArray

    val minManhattanDistanceForCrossedPoint = CrossWires.execute(input(0), input(1))
    println(s"===> Day 3a: ${minManhattanDistanceForCrossedPoint}") // 217
  }

  test("Fewest combined steps the wires must take to reach an intersectio") {
    val filePath = getClass.getClassLoader.getResource("fixtures/day3.txt").getPath
    val input = Source.fromFile(filePath).getLines.map(_.split(",")).toArray

    val steps = CrossWires.executeB(input(0), input(1))
    println(s"===> Day 3b: $steps") // 217
  }
}
