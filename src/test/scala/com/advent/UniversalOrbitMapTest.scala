package com.advent 

import org.scalatest._

class UniversalOrbitMapTest extends UnitTest {

  ignore("count indirect + direct orbit") {
    val input = 
    """
      |COM)B
      |B)C
      |C)D
      |D)E
      |E)F
      |B)G
      |G)H
      |D)I
      |E)J
      |J)K
      |K)L""".stripMargin.split("\n").filter(_.stripLineEnd.nonEmpty).map(_.split("\\)")).map { 
      case Array(x, y) => (x, y) 
      case x => { throw new IllegalArgumentException(s"Invalid input") }
    }

    val orbitMap = UniversalOrbitMap(input.toSeq)
    orbitMap.countOrbits() shouldBe 42
  }

  test("find the minimum number of orbital transfers for YOU to reach SAN") {
    val input = """
      |COM)B
      |B)C
      |C)D
      |D)E
      |E)F
      |B)G
      |G)H
      |D)I
      |E)J
      |J)K
      |K)L
      |K)YOU
      |I)SAN""".stripMargin.split("\n").filter(_.stripLineEnd.nonEmpty).map(_.split("\\)")).map { 
      case Array(x, y) => (x, y) 
      case x => { throw new IllegalArgumentException(s"Invalid input") }
    }

    val orbitMap = UniversalOrbitMap(input.toSeq)
    orbitMap.findMinimumOrbitalTransferBetween("YOU", "SAN") shouldBe 4
  }
}
