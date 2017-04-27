

object Alias {
    type RegisterFile = Map[RegisterId, Value]
    type InstructionPointer = Address

    // def nextState(curState: State): State = {
    //     curState._1
    //     curState._2
    //     curState._3
    //     cal (f, ip, mem) = curState
    //     curState match {
    //         case (f,ip, mem)=>{
    //             ...            
    //         }
    //     }
        
    }
}

import Aliases.{RegisterFile, InstructionPointer}
import Alisases._

case class Interpreter(mem:Memory){ 
    case class State(f: RegisterFile, ip: InstructionPointer){
        def nextState(): State ={
            mem(ip) match { //mem(ip) = Fetch, match = ?? 
                //rule #1  Premis: v=fetch(f,r2) +fetch(f,r3) 
                case addu(r1,r2,r3) =>{
                    val v = f(r2) +f(r3) //f = fetch                
                    State(f+(r1->v), Adress(Nat(ip.nat.i +1))) //RegFIle, InstructionPointer, Memory

                }
                //Alt rule 2
                case Beq(r1, r2, a) if f(r1)==f(r2) => {
                    State(f, a)
                }
                //Alt rule2
                case Beq(r1, r2, a) if f(r1)!=f(r2) => {
                    State(f, Adress(Nat(ip.n.i +1)))
                }
                //rule #2 & #3
                case Beq(r1, r2, a)=>{
                    if (f(r1) == f(r2)){
                        State(f, a)//Adress(Nat(ip.n.i +1)))                        
                    } else {
                        assert(f(r1) != f(r2))
                        State(f, Adress(Nat(ip.n.i+1)))
                    }
                }
            }
        }
    }
}