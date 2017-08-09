package vm

object Start {

  case class Instruction(
    index: Long, reg1: Long, reg2: Long, reg3: Long, immediateValue: Long
  ) 

  case class Result(regs: Seq[Long], state: Boolean)

  /**
   * Sample file content: {{{
   * 0x1064
   * 0x11C8
   * 0x2201
   * 0x0000
   * }}}
   */
  def main(args: Array[String]) {
    val file: String = if (null != args && !args.isEmpty) args(0) else ""

    def decode(instruction: Long): Instruction = {
      val num  =  (instruction & 0xF000) >> 12
      val reg1 =  (instruction & 0xF00 ) >>  8
      val reg2 =  (instruction & 0xF0  ) >>  4
      val reg3 =  (instruction & 0xF   ) 
      val imm  =  (instruction & 0xFF  ) 
      Instruction(num, reg1, reg2, reg3, imm)
    }

    def evaluate(instruction: Instruction): Result = 
      instruction.index match {
        case 0 => {
          println("Halt!")
          Result(Seq.empty[Long], false)
        }
        case 1 => {
          val reg1 = instruction.reg1
          val immediateValue = instruction.immediateValue
          println(s"loadi r$reg1 #$immediateValue")
          Result(Seq(immediateValue, 0, 0, 0), true)
        }
        case 2 => {
          val reg1 = instruction.reg1
          val reg2 = instruction.reg2
          val reg3 = instruction.reg3
          println(s"add r$reg1 r$reg2 r$reg3")
          Result(Seq.empty[Long], true)
        }
      }

    val lines = IO.from(file)
    // TODO: use foldLeft + takeWhile for collecting result
    lines.zipWithIndex.takeWhile { case (line, idx) =>
      val instruction = decode(java.lang.Long.decode(line))
      val result = evaluate(instruction)
      result.state
    }.toList
    println("Done!")
  }

}


