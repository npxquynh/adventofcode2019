package com.advent.calendar

import com.advent.UnitTest

import scala.io.Source
import java.io.File
import com.advent.IntCode

class Day5Test extends UnitTest {

  test("IntCode for 1202 program alarm") {
    val filePath = getClass.getClassLoader.getResource("fixtures/day5.txt").getPath
    val input = Source.fromFile(filePath).getLines.map(_.split(",")).flatten.map(_.toInt).toArray
    
    val intCode = new IntCode(input)
    val output = intCode.execute()

    println(s"==> Day 5a: ${output}")
  }
}
