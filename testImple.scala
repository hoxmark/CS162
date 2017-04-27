case class State(f: RegisterFile, ip: InstructionPinter, mem: Memory)

object Aliases{ //typedef
    type SoesialString = String
    type RgegisterFile = Map[RegisterId, Value]
}