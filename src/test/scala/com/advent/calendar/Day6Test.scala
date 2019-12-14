package com.advent.calendar

import com.advent.UnitTest

import scala.io.Source
import com.advent.UniversalOrbitMap

class Day6Test extends UnitTest {

  test("calculate indirect & direct orbits") {
    val filePath = getClass.getClassLoader.getResource("fixtures/day6.txt").getPath
    val input = Source.fromFile(filePath).getLines.filter(_.stripLineEnd.nonEmpty).map(_.split("\\)")).map { 
      case Array(x, y) => (x, y) 
      case x => { throw new IllegalArgumentException(s"Invalid input") }
    }

    val orbitMap = UniversalOrbitMap(input.toSeq)
    val result = orbitMap.countOrbits()
    println(s"==> Day 6a: $result")
  }

  test("minimum number of orbital transfers required to move from the object YOU are orbiting to the object SAN is orbiting") {
    val filePath = getClass.getClassLoader.getResource("fixtures/day6.txt").getPath
    val input = Source.fromFile(filePath).getLines.filter(_.stripLineEnd.nonEmpty).map(_.split("\\)")).map { 
      case Array(x, y) => (x, y) 
      case x => { throw new IllegalArgumentException(s"Invalid input") }
    }

    val orbitMap = UniversalOrbitMap(input.toSeq)
    val result = orbitMap.findMinimumOrbitalTransferBetween("YOU", "SAN")
    println(s"==> Day 6b: $result") // 307
  }
}
