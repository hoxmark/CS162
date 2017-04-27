case class Nat(val i: Int){
    assert(i >=0)

}
case class Value(v: Int)

sealed trait RegisterId //Sealed trait only finds extentions in this file. 
case object TreT0 extends RegisterId
case object TreT1 extends RegisterId
case object TreT2 extends RegisterId

case class Adress(n: Nat){
    def +(n2: Nat): Address = {
        Adress(n.i + n2.i)
    }
}


//AST ABstract syntax tree - 

sealed trait Instruction 
case class Addu(r1: RegisterId, r2 RegisterId, r3: RegisterId) extends Instruction

//sltu compares the contents of two specified registers. If the first is less than the second, 
//put 1 in a specified destination register, otherwise put 0
case class Sltu(r1: RegisterId, r2 RegisterId, r3: RegisterId) extends Instruction 


//beq jumps to a particular instruction if the values of two specified registers are equal

case class Beq(r1: RegisterId, r2 RegisterId, a: Adress) extends Instruction 