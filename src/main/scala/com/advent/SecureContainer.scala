package com.advent

object SecureContainer {

  object PasswordRule {
    
    def isValid(number: Int): Boolean = {
      val str = number.toString
      hasSixDigits(str) && hasTwoSameAdjacentDigits(str) && hasIncreasingDigits(str)

    }
      
    private def hasSixDigits(str: String): Boolean = str.size == 6 

    private def hasTwoSameAdjacentDigits(str: String): Boolean = {
      str.tail.foldLeft(str(0)) { (digit1, digit2) => 
        if (digit1 == digit2) return true
        digit2
      }
      return false
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