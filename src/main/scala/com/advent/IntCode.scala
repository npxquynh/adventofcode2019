package com.advent

class IntCode(input: Array[Int]) {
  val Step = 4

  def execute(): Array[Int] = {

    def helper(binaryOp: (Int, Int) => Int, baseIndex: Int) = 
      update(get(baseIndex + 3), binaryOp(get(get(baseIndex + 1)), get(get(baseIndex + 2))))

    val addMethod: (Int, Int) => Int = _ + _
    val multiplyMethod: (Int, Int) => Int = _ * _
    
    var currentIndex = 0
    var opCode = get(currentIndex)

    while(opCode != 99) {
      if (opCode == 1) helper(addMethod, currentIndex)
      else if (opCode == 2) helper(multiplyMethod, currentIndex)
      else throw new IllegalArgumentException(s"Illegal opCode $opCode")

      currentIndex = currentIndex + Step
      opCode = get(currentIndex)
    }

    input
  }

  private def get(i: Int): Int = input(i)

  private def update(i: Int, value: Int) = input.update(i, value)
}