package com.advent

sealed trait OpCode {
  val noArguments: Int = 0
}

object OpCode {
  case object Add extends OpCode {
    override val noArguments: Int = 3
  }
  case object Multiply extends OpCode {
    override val noArguments: Int = 3
  }
  case object Store extends OpCode {
    override val noArguments: Int = 1
  }
  case object Output extends OpCode {
    override val noArguments: Int = 1
  }
  case object JumpIfTrue extends OpCode {
    override val noArguments: Int = 2
  }
  case object JumpIfFalse extends OpCode {
    override val noArguments: Int = 2
  }
  case object LessThan extends OpCode {
    override val noArguments: Int = 3
  }
  case object Equals extends OpCode {
    override val noArguments: Int = 3
  }
  case object Halt extends OpCode
  case object Unknown extends OpCode

  def build(opCode: Int = 1): OpCode = opCode match {
    case 1 => Add
    case 2 => Multiply
    case 3 => Store 
    case 4 => Output
    case 5 => JumpIfTrue
    case 6 => JumpIfFalse
    case 7 => LessThan
    case 8 => Equals
    case 99 => Halt 
  }
}
sealed trait ParamMode

object ParamMode {
  case object Position extends ParamMode
  case object Immediate extends ParamMode

  def build(paramMode: Int): ParamMode = {
    if (paramMode == 0) Position
    else Immediate
  }
}

class Instruction private (val opCode: OpCode, val paramModes: Seq[ParamMode]) {

  def this(instruction: String) =
    this(
      OpCode.build(instruction.takeRight(2).toInt),
      instruction.take(instruction.length - 2).reverse.map(char => ParamMode.build(char.toString.toInt))
    )
    
  def this(instruction: Int) = this(instruction.toString)
}