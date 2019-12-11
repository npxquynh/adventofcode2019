package com.advent.calendar

import com.advent.UnitTest

import com.advent.SecureContainer

class Day4Test extends UnitTest {

  test("count how many different passwords") {
    val result = SecureContainer.countValidPassword(193651, 649729)
    println(s"===> Day 4: $result")
  }
}
