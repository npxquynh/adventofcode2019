package com.advent.calendar 

import com.advent.UnitTest

import scala.io.Source
import java.io.File
import com.advent.UnitTest
import com.advent.Universe
import com.advent.IntCode

class Day2Test extends UnitTest {

  test("IntCode for 1202 program alarm") {
    val filePath = getClass.getClassLoader.getResource("fixtures/day2.txt").getPath
    val input = Source.fromFile(filePath).getLines.map(_.split(",")).flatten.map(_.toInt).toArray
    
    // Update according to the instruction: change position 1 to 12, change positin 2 to 2
    input.update(1, 12)
    input.update(2, 2)

    val intCode = new IntCode(input)
    val output = intCode.execute()

    println(s"==> Day 2a: ${output(0)}")
  }

  test("brute force to get ouput 19690720") {
    val filePath = getClass.getClassLoader.getResource("fixtures/day2.txt").getPath
    val input = Source.fromFile(filePath).getLines.map(_.split(",")).flatten.map(_.toInt).toArray

    for (noun <- 1 to 120) {
      for (verb <- 1 to 120) {
        val inputClone = input.clone()
        inputClone.update(1, noun)
        inputClone.update(2, verb)

        val intCode = new IntCode(inputClone)
        val output = intCode.execute()

        if (output(0) == 19690720) println(s"==> Day 2b: noun = $noun, verb = $verb, result = ${100 * noun + verb}")
      }
    }

  }
}
