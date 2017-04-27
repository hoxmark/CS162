package simplescala.interpreter

object Aliases {
  type Env = Map[Variable, Value]
  type Defs = Map[FunctionName, (Variable, Exp)]
}
import Aliases.{Env, Defs}

class StuckException extends Exception

sealed trait Value
case class StrV(s: String) extends Value
case class BoolV(b: Boolean) extends Value
case class IntV(i: Int) extends Value
case object UnitV extends Value
case class ClosureV(x: Variable, e: Exp, env: Env) extends Value
case class ConstructorV(cn: ConstructorName, v: Value) extends Value
case class TupleV(vs: List[Value]) extends Value

sealed trait Kont
case class BinopLeftK(binop: Binop, e: Exp) extends Kont
case class BinopRightK(v: Value, binop: Binop) extends Kont
case class RestoreK(env: Env) extends Kont
case class AnonFunLeftK(e: Exp) extends Kont
case class AnonFunRightK(x: Variable, e: Exp, env: Env) extends Kont
case class NamedFunK(fn: FunctionName) extends Kont
case class IfK(e1: Exp, e2: Exp) extends Kont
case class BlockK(x: Variable, vals: List[Val], e: Exp) extends Kont
case class TupleK(es: List[Exp], vs: List[Value]) extends Kont
case class AccessK(n: Nat) extends Kont
case class ConstructorK(cn: ConstructorName) extends Kont
case class MatchK(cases: List[Case]) extends Kont

sealed trait Term
case class TermExp(e: Exp) extends Term
case class TermValue(v: Value) extends Term

object Interpreter {
  def makeDefs(defsSeq: Seq[Def]): Defs = {
    defsSeq.map(
      { case Def(fn, x, e) => (fn -> (x -> e)) }).toMap
  }

  def apply(defs: Defs): Interpreter = {
    new Interpreter(defs)
  }

  def apply(prog: Program): Interpreter = {
    apply(makeDefs(prog.defs))
  }

  // returns the stuck exception if it got stuck
  def runProgramToOpValue(prog: Program): Either[StuckException, Value] = {
    catchStuckException(runProgramToValue(prog))
  }

  def catchStuckException[A](f: => A): Either[StuckException, A] = {
    try {
      Right(f)
    } catch {
      case e: StuckException => Left(e)
    }
  }

  def runProgramToValue(prog: Program): Value = {
    val interpreter = apply(prog)
    interpreter.initialState(prog.e).runToValue
  }

  def usage() {
    println("Needs the name of an input SimpleScala file")
  }

  def main(args: Array[String]) {
    if (args.length != 1) {
      usage()
    } else {
      val input = SimpleScalaParser.fileContents(args(0))
      SimpleScalaParser.parseProgram(input) match {
        case Left(error) => {
          println(error)
        }
        case Right(program) => {
          //println(program)
          println(runProgramToValue(program))
        }
      }
    }
  } // main
} // Interpreter

class Interpreter(val defs: Defs) {
  def evalOp(v1: Value, binop: Binop, v2: Value): Value = {
    (v1, binop, v2) match {
      case (IntV(i1), BinopPlus, IntV(i2)) => IntV(i1 + i2)
      case (StrV(str1), BinopPlus, StrV(str2)) => StrV(str1 + str2)
      case (IntV(i1), BinopMinus, IntV(i2)) => IntV(i1 - i2)
      case (IntV(i1), BinopTimes, IntV(i2)) => IntV(i1 * i2)
      case (IntV(i1), BinopDiv, IntV(i2)) if i2 != 0 => IntV(i1 / i2)
      case (BoolV(b1), BinopAnd, BoolV(b2)) => BoolV(b1 && b2)
      case (BoolV(b1), BinopOr, BoolV(b2)) => BoolV(b1 || b2)
      case (IntV(i1), BinopLT, IntV(i2)) => BoolV(i1 < i2)
      case (IntV(i1), BinopLTE, IntV(i2)) => BoolV(i1 <= i2)
      case _ => throw new StuckException
    }
  } // evalOp

  def tupleAccess[A](as: List[A], n: Nat): A = {
    (as, n) match {
      case (a1 :: a2, Nat(1)) => a1
      case (a1 :: a2, Nat(n)) if n > 1 => tupleAccess(a2, Nat(n - 1))
      case _ => throw new StuckException
    }
  } // tupleAccess

  def constructorCaseLookup(cn: ConstructorName, cases: List[Case]): (Variable, Exp) = {
    cases match {
      case ConstructorCase(`cn`, x, e) :: _ => (x -> e)
      case _ :: rest => constructorCaseLookup(cn, rest)
      case _ => throw new StuckException
    }
  } // caseLookup

  def tupleCaseLookup(vs: List[Value], env: Env, cases: List[Case]): (Env, Exp) = {
    cases match {
      case TupCase(xs, e) :: _ if vs.size == xs.size => (useTupCase(xs, vs, env) -> e)
      case _ :: rest => tupleCaseLookup(vs, env, rest)
      case _ => throw new StuckException
    }
  } // tupleCaseLookup

  def useTupCase(xs: List[Variable], vs: List[Value], env: Env): Env = {
    (xs, vs) match {
      case (Nil, Nil) => env
      case (x1 :: x2s, v1 :: v2s) => useTupCase(x2s, v2s, env + (x1 -> v1))
      case _ => throw new StuckException
    }
  } // useTupCase

  def initialState(e: Exp): State = {
    State(TermExp(e), Map(), List())
  }
        
  case class State(t: Term, env: Env, ks: List[Kont]) {
      def runToValue(): Value = {
        var state = this
        var value = state.haltedValue
        while (value.isEmpty) {
          state = state.nextState
          value = state.haltedValue
        }
        assert(value.isDefined)
        value.get
      }

    def haltedValue(): Option[Value] = {
      (t, ks) match {
        case (TermValue(v), Nil) => Some(v)
        case _ => None
      }
    }

    // Recommendation: the first thing you should do is figure
    // out exactly which rule currently applies.  This will require
    // you to pattern match on the current term, and potentially
    // also the current continuation stack, depending on the rule.
    // Keep in mind that you can pattern match on multiple things
    // at the same time by putting them in a tuple first, e.g.:
    //
    // (first, second) match {
    //   case (Something(x), SomethingElse(y, z)) => ...
    // }
    //
    // As a hint, in our reference implementation, the longest
    // rule needs 7 lines of code to implement, and most rules
    // need only 3-4 lines of code.  If you start consistently
    // needing a lot more code than that, you should revisit your
    // general design.
    //
    // All the helper functions have been provided for you; you
    // need only implement the table in `nextState` below.
    def nextState(): State = {

      t match{
        case TermValue(tv) => (tv, ks) match {
          //rule 7
          case (v, BinopLeftK(op, e)::ks) => {
            println("Rule 7")
            State( TermExp(e) ,env,BinopRightK(v, op)::ks)
          }
          
          //Rule 8 
          case (v2, BinopRightK(v1, op)::ks) => {
            println("Rule 8")
            State(TermValue(evalOp(v1, op, v2)) ,env, ks)
          }
          //Rule 11
          case (ClosureV(xP,eP,envP), AnonFunLeftK(e)::ks) => {
            println("Rule 11")
            State(TermExp(e) ,env, AnonFunRightK(xP, eP, envP) :: ks)
          }
          // Rule 12
          case (v, AnonFunRightK(xP, eP, envP)::ks) => {
            println("Rule 12")
            State(TermExp(eP), (envP + (xP -> v)), RestoreK(env) :: ks)
          }
          //rule 14 
          case (v, NamedFunK(fn)::ks) => {
            val (x, e) = defs(fn)
            State(TermExp(e), (Map[Variable, Value]() + (x -> v)), RestoreK(env)::ks)
          }
          
          //rule 15
          case (v, RestoreK(envP)::k2) => {
            State(TermValue(v), envP, k2)
          }

          //Rule 22
          case (BoolV(true), IfK(e1,e2)::k2) => State(TermExp(e1), env, k2)
           
          //Rule 23
          case (BoolV(false), IfK(e1,e2)::k2) => State(TermExp(e2), env, k2)

          // Rule 24
          case (v, BlockK(x1,  Val(x2,e1)::vals, e2)::k2) => {
            State(TermExp(e1), (env + (x1->v)), BlockK(x2, vals, e2):: RestoreK(env):: k2)
          } 

          // Rule 25  TODO: SWAP OUT LIST WITH SOMETHING
          case (v, BlockK(x, List() , e)::k2) => State(TermExp(e), (env+(x->v)),RestoreK(env)::k2)

          // Rule 26
          case (v1, (TupleK(e1 :: e2, v2)::k2)) => State(TermExp(e1), env, TupleK(e2, v1 :: v2)::k2)

          //Rule 27 TODO MAKE es EMPLTY? 
          case (v1, (TupleK(List(), v2)::k2)) => {
            val v3Temp = (v1 :: v2)
            val v3 = v3Temp.reverse
            State(TermValue(TupleV(v3)), env, k2)
          }

          //rule 28
          case (TupleV(v1), AccessK(n)::k2) => {
            val v2 = tupleAccess(v1, n)
            State(TermValue(v2), env, k2)
          }

          //rule 29
          case (v, ConstructorK(cn)::k2) => State(TermValue(ConstructorV(cn, v)), env, k2)

          //Rule 30 
          case (ConstructorV(cn, v), MatchK(cas)::k2) => {
            val (x, e) = constructorCaseLookup(cn, cas)
            State(TermExp(e), (env + (x->v)), RestoreK(env)::k2)
          }

          //rule 31
          case (TupleV(v1), matchK(cas)::k2)=> {
            val (envP, e) = tupleCaseLookup(v1, env, cas)
            State(TermExp(e), envP, RestoreK(env)::k2)
          } 

        }

        case TermExp(te) => (te, ks) match { 
          // Rule 1
          case (StringExp(str), _) => State(TermValue(StrV(str)),env,ks)
        
          // Rule 2
          case (BooleanExp(b), _) => State(TermValue(BoolV(b)),env,ks)
         
          // Rule 3
          case (IntExp(b), _) => State(TermValue(IntV(b)),env,ks)
         
          // Rule 4
          case (UnitExp, _) => State(TermValue(UnitV),env,ks)
          
          // Rule 5 Need to handle premis
          case (VariableExp(x), premis) if env.contains(x) => State(TermValue(env(x)),env,ks)
          
          // Rule 6 Term or TermExp
          case (BinopExp(e1, op, e2), _) =>  State(TermExp(e1),env,BinopLeftK(op, e2)::ks)
          // 
          // Rule 9 
          case (FunctionExp(x, e), _) => State(TermValue(ClosureV(x, e, env)), env, ks)
          
          //rule 10
          case (AnonCallExp(e1, e2) ,_) => State(TermExp(e1),env, AnonFunLeftK(e2)::ks)
          
          // Rule 13 
          // TODO ADD PREMIST TO RULE 13 type Defs = Map[FunctionName, (Variable, Exp)]          
          case (NamedCallExp(fn, e), _) => State(TermExp(e), env, NamedFunK(fn)::ks) 

          //Rule 16
          case (IfExp(e1, e2, e3), _ ) => State(TermExp(e1), env, IfK(e2, e3)::ks)
          
          //rule 17
          case (BlockExp(Val(x,e1):: vals, e2),_) => State(TermExp(e1), env, BlockK(x, vals,e2)::ks)

          // rule 18         
          case (TupleExp(e1 :: e2),_) => State(TermExp(e1), env, TupleK(e2,  List() )::ks)

          //Tule 19
          case (AccessExp(e, n),_) => State(TermExp(e), env, AccessK(n)::ks)

          //Tule 20
          case (ConstructorExp(cn, e), _) => State(TermExp(e), env, ConstructorK(cn)::ks)

          //rule 21
          case (MatchExp(e, cases), _) => State(TermExp(e), env, MatchK(cases)::ks)

          

        }


      }  
      

      // State(t, env, ks)

    } // nextState
  } // State
} // Interpreter

// sealed trait Value
// case class StrV(s: String) extends Value
// case class BoolV(b: Boolean) extends Value
// case class IntV(i: Int) extends Value
// case object UnitV extends Value
// case class ClosureV(x: Variable, e: Exp, env: Env) extends Value
// case class ConstructorV(cn: ConstructorName, v: Value) extends Value
// case class TupleV(vs: List[Value]) extends Value

// sealed trait Kont
// case class BinopLeftK(binop: Binop, e: Exp) extends Kont
// case class BinopRightK(v: Value, binop: Binop) extends Kont
// case class RestoreK(env: Env) extends Kont
// case class AnonFunLeftK(e: Exp) extends Kont
// case class AnonFunRightK(x: Variable, e: Exp, env: Env) extends Kont
// case class NamedFunK(fn: FunctionName) extends Kont
// case class IfK(e1: Exp, e2: Exp) extends Kont
// case class BlockK(x: Variable, vals: List[Val], e: Exp) extends Kont
// case class TupleK(es: List[Exp], vs: List[Value]) extends Kont
// case class AccessK(n: Nat) extends Kont
// case class ConstructorK(cn: ConstructorName) extends Kont
// case class MatchK(cases: List[Case]) extends Kont

// sealed trait Term
// case class TermExp(e: Exp) extends Term
// case class TermValue(v: Value) extends Term