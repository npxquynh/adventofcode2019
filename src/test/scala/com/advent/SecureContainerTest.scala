package com.advent

import org.scalatest._
import scala.io.Source
import java.io.File
import com.advent.SecureContainer.PasswordRule

class SecureContainerTest extends UnitTest {

  test("PasswordRule") {
    PasswordRule.isValid(111111) shouldBe true
    PasswordRule.isValid(122345) shouldBe true

    PasswordRule.isValid(111) shouldBe false
    PasswordRule.isValid(123456) shouldBe false
    PasswordRule.isValid(123245) shouldBe false
  }
}
