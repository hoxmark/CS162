package simplescala.polytyped

class IllTyped(val inTypeOf: Boolean) extends Exception {
  def this() {
    this(true)
  }
}

object Aliases {
  type NamedFunctionDefs = Map[FunctionName, (List[TypeVariable], Type, Type)]
  type TypeDefs = Map[UserDefinedTypeName, (List[TypeVariable], Map[ConstructorName, Type])]
  type ConstructorDefs = Map[ConstructorName, UserDefinedTypeName]
  type TypeEnv = Map[Variable, Type]
  type TypeVarsInScope = Set[TypeVariable]
}
import Aliases._

object Typechecker {
  def ensureSet[A](items: Seq[A]) {
    if (items.toSet.size != items.size) {
      throw new IllTyped(false)
    }
  }

  def ensureTypeVarsInScope(ts: Seq[TypeVariable], tau: Type) {
    val asSet = ts.toSet
    def recur(tau: Type) {
      tau match {
        case StringType | BooleanType | IntegerType | UnitType => ()
        case FunctionType(tau1, tau2) => {
          recur(tau1)
          recur(tau2)
        }
        case TupleType(taus) => taus.foreach(recur)
        case UserType(_, taus) => taus.foreach(recur)
        case TypeVariableType(t) if asSet.contains(t) => ()
        case _ => throw new IllTyped(false)
      }
    } // recur

    recur(tau)
  } // ensureTypeVarsInScope

  def makeMap[K, V](ks: List[K], vs: List[V]): Map[K, V] = {
    (ks, vs) match {
      case (Nil, Nil) => Map()
      case (k1 :: k2s, v1 :: v2s) => {
        (makeMap(k2s, v2s)) + (k1 -> v1)
      }
      case _ => throw new IllTyped(false)
    }
  } // makeMap

  def typeReplace(ts: List[TypeVariable], taus1: List[Type], tau2: Type): Type = {
   typeReplaceHelper(makeMap(ts, taus1), tau2)


  } // typeReplace

  def typeReplaceHelper(m: Map[TypeVariable, Type], tau: Type): Type = {
     tau match {
      case StringType => StringType
      case BooleanType => BooleanType
      case IntegerType => IntegerType
      case UnitType => UnitType
      case FunctionType(t1,t2) => 
        val tt1 = typeReplaceHelper(m, t1)
        val tt2 = typeReplaceHelper(m, t2)
        FunctionType(tt1,tt2)
      case TupleType(t1) => 
        val tt1 = typeReplaceHelperList(m, t1)
        TupleType(tt1)
      case UserType(UserDefinedTypeName(un), t1) => 
        val tt1 = typeReplaceHelperList(m, t1)
        UserType(UserDefinedTypeName(un), tt1)

      case TypeVariableType(t) => m(t)
      case _ => throw new IllTyped(false)
    }
    
  } // typeReplaceHelper

  def typeReplaceHelperList(m: Map[TypeVariable, Type], taus: List[Type]): List[Type] = {
     taus match {
       case List() => List()
       case (t1::t2) => 
          val tt1 = typeReplaceHelper(m, t1)
          val tt2 = typeReplaceHelperList(m, t2)
          tt1 :: tt2
     }

  } // typeReplaceHelperList

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

  def asSingleton[A](as: List[A]): A = {
    as match {
      case a :: Nil => a
      case a1 :: a2 :: rest if a1 == a2 => {
        asSingleton(a2 :: rest)
      }
      case _ => throw new IllTyped(false)
    }
  } // asSingleton

  def apply(p: Program): Type = {
    ensureSet(p.defs.map(_.fn))
    p.defs.foreach(d => ensureSet(d.ts))
    ensureSet(p.tdefs.map(_.un))
    ensureSet(p.tdefs.flatMap(_.cdefs.map(_.cn)))
    p.tdefs.foreach(td => ensureSet(td.ts))
    p.tdefs.foreach(td =>
      td.cdefs.foreach(cd =>
        ensureTypeVarsInScope(td.ts, cd.tau)))
    p.defs.foreach(d => {
      ensureTypeVarsInScope(d.ts, d.tau1)
      ensureTypeVarsInScope(d.ts, d.tau2)
    })
      
    val fdefs = p.defs.map( { case Def(fn, ts, _, tau1, tau2, _) => (fn -> (ts, tau1, tau2)) } ).toMap
    val tdefs = p.tdefs.map(
      { case UserDefinedTypeDef(un, ts, cdefs) =>
          (un -> (ts -> cdefs.map(
            { case ConstructorDefinition(cn, tau) => (cn -> tau) }).toMap)) }).toMap
    val cdefs = p.tdefs.flatMap(
      { case UserDefinedTypeDef(un, _, cdefs) => cdefs.map(
          { case ConstructorDefinition(cn, _) => (cn -> un) } ) } ).toMap
    val checker = new Typechecker(fdefs, tdefs, cdefs)
    p.defs.foreach(checker.checkDef)
    checker.typeof(p.e)
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
  import Typechecker._

  def checkDef(theDef: Def) {
    new InTypeScope(theDef.ts.toSet).checkDef(theDef)
  }

  def typeof(exp: Exp): Type = {
    new InTypeScope(Set()).typeof(exp, Map())
  }

  class InTypeScope(val tscope: TypeVarsInScope) {
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
    // needs 19 lines of code to implement, and most rules need only
    // 3 - 8 lines of code.  If you start consistently needing a lot
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
        // case (NamedCallExp(fn, e), gam) if fdefs.contains(fn) => 
        //   val tup = fdefs(fn) 
        //   val tau1 = typeof(e, gam)
          
        //   if (tau1 == tup._1) tup._2
        //   else throw new IllTyped(false)
        
        // //nameCall TODO, add erro rhandling 
        case (NamedCallExp(fn, tau1, e), gam) if fdefs.contains(fn) => 
          if (typeOkList(tau1)){
            val tup = fdefs(fn) 
            // val tau2pp = typeof(e, gam)
            val tau2p = typeReplace(tup._1, tau1, tup._2)
            val tau3p = typeReplace(tup._1, tau1, tup._3)


            tau3p

          } else {
            throw new IllTyped(false)
          }
          
          
      
      //   //Tup
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

      //   //Constructor 
        // case (ConstructorExp(cn, e), gam) if cdefs.contains(cn) =>
        //   val tau = typeof(e, gam)
        //   val un = cdefs(cn) 
        //   if ((tdefs(un))(cn)==tau)  
        //   UserType(un)
        //   else throw new IllTyped(false)      
        
        case (ConstructorExp(cn, taus, e), gam) if (cdefs.contains(cn) & typeOkList(taus))=>
          val tau = typeof(e, gam)
          val un = cdefs(cn) 

          val tup = tdefs(un)
          val bigT = tup._1
          val m = tup._2
          val tau2 = m(cn)
          val tau2p = typeReplace(bigT, taus, tau2)

          //Sjekk om Lengden av storeT er like lengden til taus/ tau1
          //

          // if ((tdefs(un))(cn)==tau)  
          UserType(un, taus)
          // else throw new IllTyped(false)      
      
        // Match-up
        // case (MatchExp(e1, cases), gam) => 
        //   (typeof(e1, gam)) match {
        //     case TupleType(tau1) => 
        //       cases match {             
        //         case TupCase(xs, e3) :: List() => 
        //           if (xs.length != tau1.length) throw new IllTyped(false)
        //           else {
        //             val gamp = tupGamma(xs ,tau1, gam )
        //             typeof(e3, gamp)                  
        //           }
        //         case _ => throw new IllTyped(false)
        //       }
              
        //     case UserType(un) if tdefs.contains(un) =>
        //       casesSane(cases, un)
        //       val tau11 = casesTypes(cases, gam, tdefs(un))
        //       val tau12 = asSingleton(tau11)
        //       tau12             
        //     case _ => throw new IllTyped(false) 
        //   }        
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
              
            case UserType(un, taus) if tdefs.contains(un) =>
              casesSane(cases, un)
              val tup = tdefs(un)
              val bigT = tup._1
              val m = tup._2
              val tau2 = casesTypes(cases, gam, bigT, taus, m)
              val tau3 = asSingleton(tau2)
              tau3             
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
    } // tupleTypes

    def casesTypes(cases: List[Case], gamma: TypeEnv, ts: List[TypeVariable], taus1: List[Type], m: Map[ConstructorName, Type]): List[Type] = {
      cases match {
        case Nil => Nil
        case ConstructorCase(cn, x, e) :: cases => {
          val tau2 = m(cn)
          val tau2P = typeReplace(ts, taus1, tau2)
          val tau3 = typeof(e, gamma + (x -> tau2P))
          tau3 :: casesTypes(cases, gamma, ts, taus1, m)
        }
        case _ => throw new IllTyped(false)
      }
    } // casesTypes

    def typeOkList(taus: List[Type]): Boolean = {
      taus.forall(typeOk)
    } // typeOkList

    def typeOk(tau: Type): Boolean = {
      tau match {
        case StringType | BooleanType | IntegerType | UnitType => true
        case FunctionType(tau1, tau2) => typeOk(tau1) && typeOk(tau2)
        case TupleType(taus) => typeOkList(taus)
        case UserType(_, taus) => typeOkList(taus)
        case TypeVariableType(tv) => tscope.contains(tv)
      }
    } // typeOk
  } // InTypeScope

  def casesSane(cases: List[Case], un: UserDefinedTypeName): Boolean = {
    val (ts, m) = tdefs(un)
    casesSaneHelper(cases, Set(), m)
  } // casesSane
} // Typechecker
