package com.advent

import org.scalatest._
import scala.io.Source
import java.io.File

class CrossWiresTest extends UnitTest {

  test("build WirePath") {
    val input: Seq[String] = "R2,U1,L1,D3".split(",")
    val wirePath = new WirePath

    val expectedPaths = Set(
      Coordinate(1, 0),
      Coordinate(2, 0),
      Coordinate(2, 1),
      Coordinate(1, 1),
      Coordinate(1, -1),
      Coordinate(1, -2)
    )
    WirePath.build(input) shouldBe expectedPaths
  }

  test("Manhattan distance for cross wires 1") {
    val input1 = "R75,D30,R83,U83,L12,D49,R71,U7,L72".split(",")
    val input2 = "U62,R66,U55,R34,D71,R55,D58,R83".split(",")
    
    CrossWires.execute(input1, input2) shouldBe 159
  }

  test("Manhattan distance for cross wires 2") {
    val input1 = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51".split(",")
    val input2 = "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7".split(",")
    
    CrossWires.execute(input1, input2) shouldBe 135
  }
}
