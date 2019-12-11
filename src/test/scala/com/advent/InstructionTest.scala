package com.advent

import org.scalatest._

class InstructionTest extends UnitTest {

  test("creates Instruction from string input") {
    val instruction = new Instruction("1002")

    instruction.opCode shouldBe OpCode.Multiply
    instruction.paramModes shouldBe Seq(ParamMode.Position, ParamMode.Immediate)
  }
}
