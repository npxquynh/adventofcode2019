package com.advent 

import org.scalatest._

class UniverseTest extends UnitTest {

  test("fuelToLaunch") {
    Universe.fuelToLaunch(14) shouldBe 2
    Universe.fuelToLaunch(1969) shouldBe 966
    Universe.fuelToLaunch(100756) shouldBe 50346
  }
}
