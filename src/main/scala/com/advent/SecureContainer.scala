package com.advent

object SecureContainer {

  object PasswordRule {
    
    def isValid(number: Int): Boolean = {
      val str = number.toString
      hasSixDigits(str) && followAdjacentDigitRule(str) && hasIncreasingDigits(str)
    }
      
    private def hasSixDigits(str: String): Boolean = str.size == 6 

    private def followAdjacentDigitRule(str: String): Boolean = {
      val sameDigitCount = 1

      val result = str.tail.foldLeft((sameDigitCount, str(0))) { (X, nextDigit) => 
        if (X._2 == nextDigit) (X._1 + 1, nextDigit)
        else if (X._1 == 2) return true
        else (sameDigitCount, nextDigit)
      }

      if (result._1 == 2) true else false
    }

    private def hasIncreasingDigits(str: String): Boolean = {
      str.tail.foldLeft(str(0)) { (digit1, digit2) => 
        if (digit1 > digit2) return false
        digit2
      }

      return true
    }
  }

  def countValidPassword(min: Int, max: Int): Int = {
    (min to max).filter(PasswordRule.isValid).size
  }
}