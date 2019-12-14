package com.advent

import org.scalatest._
import com.advent.SecureContainer.PasswordRule

class SecureContainerTest extends UnitTest {

  test("PasswordRule") {
    PasswordRule.isValid(122345) shouldBe true

    PasswordRule.isValid(111) shouldBe false
    PasswordRule.isValid(123456) shouldBe false
    PasswordRule.isValid(123245) shouldBe false
  }

  test("PasswordRule: two adjacent matching digits are not part of a larger group of matching digits") {
    PasswordRule.isValid(112233) shouldBe true 
    PasswordRule.isValid(112222) shouldBe true
    PasswordRule.isValid(111122) shouldBe true 
    
    PasswordRule.isValid(123444) shouldBe false 
  }
}
