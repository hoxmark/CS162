package simplescala.simplytyped

class IllTyped(val inTypeOf: Boolean) extends Exception {
  def this() {
    this(true)
  }
}

object Aliases {
  type NamedFunctionDefs = Map[FunctionName, (Type, Type)]
  type TypeDefs = Map[UserDefinedTypeName, Map[ConstructorName, Type]]
  type ConstructorDefs = Map[ConstructorName, UserDefinedTypeName]
  type TypeEnv = Map[Variable, Type]
}
import Aliases._

object Typechecker {
  def ensureSet[A](items: Seq[A]) {
    if (items.toSet.size != items.size) {
      throw new IllTyped(false)
    }
  }

  def apply(p: Program): Type = {
    ensureSet(p.defs.map(_.fn))
    ensureSet(p.tdefs.map(_.un))
    ensureSet(p.tdefs.flatMap(_.cdefs.map(_.cn)))

    val fdefs = p.defs.map( { case Def(fn, _, tau1, tau2, _) => (fn -> (tau1 -> tau2)) } ).toMap
    val tdefs = p.tdefs.map(
      { case UserDefinedTypeDef(un, cdefs) => (un -> cdefs.map(
          { case ConstructorDefinition(cn, tau) => (cn -> tau) } ).toMap) } ).toMap
    val cdefs = p.tdefs.flatMap(
      { case UserDefinedTypeDef(un, cdefs) => cdefs.map(
          { case ConstructorDefinition(cn, _) => (cn -> un) } ) } ).toMap
    val checker = new Typechecker(fdefs, tdefs, cdefs)
    p.defs.foreach(checker.checkDef)
    checker.typeof(p.e, Map())
  } // apply

  def eitherType(p: Program): Either[IllTyped, Type] = {
    try {
      Right(apply(p))
    } catch {
      case e: IllTyped => Left(e)
    }
  }
} // Typechecker

class Typechecker(val fdefs: NamedFunctionDefs,
                  val tdefs: TypeDefs,
                  val cdefs: ConstructorDefs) {
  def checkDef(theDef: Def) {
    if (typeof(theDef.e, Map(theDef.x -> theDef.tau1)) != theDef.tau2) {
      throw new IllTyped(false)
    }
  }
 
  // Recommendation: the first thing you should do is figure
  // out exactly which rule currently applies.  This will
  // require you to pattern match on the expression, and potentially
  // check additional things as well.
  //
  // As a hint, in our reference implementation, the longest rule
  // needs 13 lines of code to implement, and most rules need only
  // 3 - 7 lines of code.  If you start consistently needing a lot
  // more code than that, you should revisit your general design.
  //
  // All the helper functions have been provided for you; you need only
  // implement the typing rules in `typeof` below.
  def typeof(exp: Exp, gamma: TypeEnv): Type = {
    
    // println("exp:"+exp) 
    (exp, gamma) match {
      case (VariableExp(x), gam) if gam.contains(x) => gam(x)  
      case (StringExp(s: String), gam) => StringType
      case (BooleanExp(b: Boolean), gam) => BooleanType
      case (IntExp(i: Int), gam) => IntegerType
      case (UnitExp, gam) => UnitType

      case (BinopExp(e1, op, e2), gam) =>
        (typeof(e1, gam), op ,typeof(e2, gam)) match {
          case (IntegerType, BinopPlus, IntegerType) => IntegerType   //+int
          case (StringType, BinopPlus, StringType) => StringType      //+string       
          case (IntegerType, BinopMinus, IntegerType) => IntegerType  //arithOp       
          case (IntegerType, BinopDiv, IntegerType) => IntegerType    //arithOp              
          case (IntegerType, BinopTimes, IntegerType) => IntegerType  //arithOp
          case (BooleanType, BinopAnd, BooleanType ) => BooleanType   //boolOp
          case (BooleanType, BinopOr, BooleanType ) => BooleanType    //boolOp
          case (IntegerType, BinopLT, IntegerType) => BooleanType     //RelOp
          case (IntegerType, BinopLTE, IntegerType) => BooleanType    //RelOp
          case _ => throw new IllTyped(false)
        }        

      // If
      case (IfExp(e1, e2, e3), gam) => 
      (typeof(e1,gam), typeof(e2, gam), typeof(e3, gam)) match {
        case (BooleanType, x, y) if (x==y) => x
        
        case _ =>{
          throw new IllTyped(false)
        }
      }
      
      //Anonfunc
      case (FunctionExp(x, tau, e), gam) =>           
        FunctionType(tau, typeof(e, (gam + (x -> tau))))   
      
      //ANON call    
      case (AnonCallExp(e1, e2), gam) =>
        (typeof(e1, gam), typeof(e2, gam)) match {
          case (FunctionType(tau1, tau2), tau3) if tau1 == tau3 => tau2
          case _ => throw new IllTyped(false)
        }

      //nameCall
      case (NamedCallExp(fn, e), gam) if fdefs.contains(fn) => 
        val tup = fdefs(fn) 
        val tau1 = typeof(e, gam)
        if (tau1 == tup._1) tup._2
        else throw new IllTyped(false)
    
      //Tup
      case (TupleExp(es), gam) => TupleType(tupleTypes(es, gam))

      //Acc
      case (AccessExp(e, n), gam) => 
      (typeof(e,gam), n) match {
        case (TupleType(t), Nat(nn))  => tupleAccess(t, n)
        case _ => throw new IllTyped(false)
      }

      //Block 
      case (BlockExp(vals, e), gam) =>
        val gamP = blockGamma(vals, gam)
        typeof(e, gamP)

      //Constructor 
      case (ConstructorExp(cn, e), gam) if cdefs.contains(cn) =>
        val tau = typeof(e, gam)
        val un = cdefs(cn) 
        if ((tdefs(un))(cn)==tau)  
        UserType(un)
        else throw new IllTyped(false)      
    
      //Match-up
      // cases: List(TupCase(List(Variable(x), Variable(y), Variable(z)),VariableExp(Variable(x))))
      case (MatchExp(e1, cases), gam) => 
        (typeof(e1, gam)) match {
          case TupleType(tau1) => 
            cases match {             
              case TupCase(xs, e3) :: List() => 
                if (xs.length != tau1.length) throw new IllTyped(false)
                else {
                  val gamp = tupGamma(xs ,tau1, gam )
                  typeof(e3, gamp)                  
                }
              case _ => throw new IllTyped(false)
            }
            
          case UserType(un) if tdefs.contains(un) =>
            casesSane(cases, un)
            val tau11 = casesTypes(cases, gam, tdefs(un))
            val tau12 = asSingleton(tau11)
            tau12             
          case _ => throw new IllTyped(false) 
        }        
      case _ => throw new IllTyped(false) 
    } 
  } // typeof
        
  def blockGamma(vals: List[Val], gamma: TypeEnv): TypeEnv = {
    vals match {
      case Nil => gamma
      case Val(x, e) :: vals => {
        val tau = typeof(e, gamma)
        blockGamma(vals, gamma + (x -> tau))
      }
    }
  } // blockGamma

  def tupleTypes(exps: List[Exp], gamma: TypeEnv): List[Type] = {
    exps match {
      case Nil => Nil
      case e1 :: e2s => {
        typeof(e1, gamma) :: tupleTypes(e2s, gamma)
      }
    }
  }

  def tupleAccess[A](as: List[A], n: Nat): A = {
    (as, n) match {
      case (a1 :: a2, Nat(1)) => a1
      case (a1 :: a2, Nat(n)) if n > 1 => tupleAccess(a2, Nat(n - 1))
      case _ => throw new IllTyped(false)
    }
  } // tupleAccess

  def tupGamma(xs: List[Variable], taus: List[Type], gamma: TypeEnv): TypeEnv = {
    (xs, taus) match {
      case (Nil, Nil) => gamma
      case (x1 :: x2, tau1 :: tau2) => {
        tupGamma(x2, tau2, gamma + (x1 -> tau1))
      }
      case _ => throw new IllTyped(false)
    }
  } // tupGamma

  def casesSane(cases: List[Case], un: UserDefinedTypeName): Boolean = {
    casesSaneHelper(cases, Set(), tdefs(un))
  } // casesSane

  def casesSaneHelper(cases: List[Case], cnBar: Set[ConstructorName], m: Map[ConstructorName, Type]): Boolean = {
    cases match {
      case Nil => m.keySet == cnBar
      case ConstructorCase(cn1, x, e) :: cases => {
        (m.contains(cn1) &&
         !cnBar.contains(cn1) &&
         casesSaneHelper(cases, cnBar + cn1, m))
      }
      case _ => false
    }
  } // casesSaneHelper

  def casesTypes(cases: List[Case], gamma: TypeEnv, m: Map[ConstructorName, Type]): List[Type] = {
    cases match {
      case Nil => Nil
      case ConstructorCase(cn, x, e) :: cases => {
        val tau1 = m(cn)
        val tau2 = typeof(e, gamma + (x -> tau1))
        tau2 :: casesTypes(cases, gamma, m)
      }
      case _ => throw new IllTyped(false)
    }
  } // casesTypes

  def asSingleton[A](as: List[A]): A = {
    as match {
      case a :: Nil => a
      case a1 :: a2 :: rest if a1 == a2 => {
        asSingleton(a2 :: rest)
      }
      case _ => throw new IllTyped(false)
    }
  } // asSingleton
} // Typechecker
