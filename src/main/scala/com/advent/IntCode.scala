package com.advent

import com.advent.OpCode._

class IntCode(input: Array[Int]) {
  case class Param(mode: ParamMode, value: Int)

  def execute(): Array[Int] = {
    var index = 0
    var flag = true
    while (flag) {
      val instruction = new Instruction(input(index))
      index += 1

      if (instruction.opCode == Halt) flag = false
      
      process(instruction, index) 
      index += instruction.opCode.noArguments
    }

    input
  }

  private val addFn: (Int, Int) => Int = _ + _
  private val multiplyFn: (Int, Int) => Int = _ * _

  private def process(instruction: Instruction, index: Int): Unit = {
    val params = getParams(instruction, index)
    assert(params.length == instruction.opCode.noArguments)

    instruction.opCode match {
      case Add      => binaryOp(params, addFn)
      case Multiply => binaryOp(params, multiplyFn)
      case Store => store(params.head)
      case Output => println(params)
      case _ => ()
    }
  }

  private def getParams(instruction: Instruction, index: Int): Seq[Param] = {
    val rawParams = (index until (index + instruction.opCode.noArguments)).map(input(_))
    assert(rawParams.length >= instruction.paramModes.length)
    instruction.paramModes.zipAll(rawParams, ParamMode.Position, 0).map(x => Param(x._1, x._2))
  }

  private def binaryOp(params: Seq[Param], fn: (Int, Int) => Int) = {
    val result = fn(getValue(params(0)), getValue(params(1)))
    setValue(params(2), result)
  }

  private def store(param: Param): Unit = 
    setValue(param, param.value)

  private def getValue(param: Param): Int =
    if (param.mode == ParamMode.Position) input(param.value)
    else param.value

  private def setValue(param: Param, newValue: Int): Unit =
    input.update(param.value, newValue)
}
