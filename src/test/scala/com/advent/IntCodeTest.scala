package com.advent 

import org.scalatest._

class IntCodeTest extends UnitTest {

  test("execute IntCode program") {
    val input: Array[Int] = Array(1,9,10,3,2,3,11,0,99,30,40,50)
    val intCode = new IntCode(input)

    intCode.execute shouldBe Array(3500,9,10,70, 2,3,11,0, 99, 30,40,50)
  }

  test("outputs 0 if input is 0, immediate mode") {
    // This is not really a test
    val input = Array(3,3,1105,-1,9,1101,0,0,12,4,12,99,1)
    val intCode = new IntCode(input)

    intCode.execute
  }

  test("outputs 0 if input is 0, position mode") {
    // This is not really a test
    val input = Array(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9)
    val intCode = new IntCode(input)

    intCode.execute
  }

  test("position mode: consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not") {
    val input = Array(3,9,8,9,10,9,4,9,99,-1,8)
    new IntCode(input).execute()
  }

  test("immediate mode: consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not)") {
    val input = Array(3,3,1108,-1,8,3,4,3,99)
    new IntCode(input).execute()
  }

  test("larger input") {
    val input = Array(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
    1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
    999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99)
    val intCode = new IntCode(input)

    intCode.execute()
  }
}
