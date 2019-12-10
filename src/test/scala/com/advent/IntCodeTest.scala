package com.advent 

import org.scalatest._
import scala.io.Source
import java.io.File

class IntCodeTest extends UnitTest {

  test("execute IntCode program") {
    val input: Array[Int] = Array(1,9,10,3,2,3,11,0,99,30,40,50)
    val intCode = new IntCode(input)

    intCode.execute shouldBe Array(3500,9,10,70, 2,3,11,0, 99, 30,40,50)
  }
}
