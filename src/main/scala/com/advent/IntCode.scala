package com.advent

import com.advent.OpCode._

class IntCode(input: Array[Int]) {
  private var index = 0

  case class Param(mode: ParamMode, value: Int)

  def execute(): Array[Int] = {
    var flag = true
    while (flag) {
      val instruction = new Instruction(input(index))
      if (instruction.opCode == Halt) flag = false
      else
       process(instruction, index) 
    }

    input
  }

  private val addFn: (Int, Int) => Int = _ + _
  private val multiplyFn: (Int, Int) => Int = _ * _
  private val lessThanFn: (Int, Int) => Boolean = _ < _
  private val equalsFn: (Int, Int) => Boolean = _ == _

  private def process(instruction: Instruction, instructionIndex: Int): Unit = {
    val params = getParams(instruction, instructionIndex)
    assert(params.length == instruction.opCode.noArguments)

    // println(instruction.opCode, instruction.paramModes, params)

    println(instruction.opCode)
    println(input.mkString(","))

    instruction.opCode match {
      case Add      => binaryOp(params, addFn)
      case Multiply => binaryOp(params, multiplyFn)
      case Store => store(params.head)
      case Output => println(s"==> ${getValue(params.head)}")
      case JumpIfTrue => jumpIfTrue(params, instructionIndex)
      case JumpIfFalse => jumpIfFalse(params, instructionIndex)
      case LessThan => predicateOp(params, lessThanFn)
      case Equals => predicateOp(params, equalsFn)
      case _ => ()
    }

    val values = params.map(getValue(_))
    instruction.opCode match {
      case JumpIfFalse if (values(0) == 0) => index = values(1)
      case JumpIfTrue if(values(0) != 0) => index = values(1)
      case _ => index += (1 + instruction.opCode.noArguments)
    }
  }

  private def getParams(instruction: Instruction, instructionIndex: Int): Seq[Param] = {
    val index = instructionIndex + 1
    val rawParams = (index until (index + instruction.opCode.noArguments)).map(input(_))
    assert(rawParams.length >= instruction.paramModes.length)
    instruction.paramModes.zipAll(rawParams, ParamMode.Position, 0).map(x => Param(x._1, x._2))
  }

  private def binaryOp(params: Seq[Param], fn: (Int, Int) => Int) = {
    val values = params.map(getValue(_))
    val result = fn(values(0), values(1))
    setValue(params(2), result)
  }

  private def store(param: Param): Unit = {
    println("Input some value:")
    val inputValue = scala.io.StdIn.readInt
    setValue(param, inputValue)
  }

  private def jumpIfTrue(params: Seq[Param], instructionIndex: Int) = {
    val values = params.map(getValue(_))
    if (values(0) != 0) setValue(instructionIndex, values(1))
  }

  private def jumpIfFalse(params: Seq[Param], instructionIndex: Int) = {
    val values = params.map(getValue(_))
    if (values(0) == 0) setValue(instructionIndex, values(1))
  }

  private def predicateOp(params: Seq[Param], fn: (Int, Int) => Boolean) = {
    val values = params.map(getValue)
    if (fn(values(0), values(1))) setValue(params(2), 1)
    else setValue(params(2), 0)
  }

  private def getValue(param: Param): Int =
    if (param.mode == ParamMode.Position) input(param.value)
    else param.value

  private def setValue(param: Param, newValue: Int): Unit = setValue(param.value, newValue)

  private def setValue(index: Int, newValue: Int) = input.update(index, newValue)
}
