//Map() // sase as rho element of Env = Variable -> Value


// case _ => throw new stuckExeption //Depends on the header matching. 

// if you are pattermatching on something that does not make sence it will 

//Patternmatch on a string in simpleScala

//Some ( 7 ) wold be a constructor("Some" , intV(7)

//Patternmatcing can very easy ask the question to a value -> "What are you"
//Patternmatching and higher order function is what poeple strugle with in Scala. 
def test (term: Term){
    term match {
        case TermExp(e) => ...
            e match {
                StringExo(str) => 
            }
        case TermValue(v) => ...
    }
}

//{val x = 7 ({val x = 8 (8)})}

//Rule 14
//def foo(x) = e
    foo(7)

    [x-> 7] 

    the restoreK()

    //If the enviroment does not change we do not need to restore anything. 

    //rule 15. 
    //Term: v its a value, string or int or what ever. 
    //Premise: if kArrow = restoreK(p')::k2Arrow. 
    If you have a value there, it means you are done with that value. 
    //15, now you are done with what you were doing -> now get the new job. Restore to that enviroment. 
    //theis pops the top of the top , keep executing with whatever is left on the stack. 

    ///match on a touple. Look at the tool(?) and the continuation stack .

// Ikke gjør dette
    t match {
        ks match {
            
        }
    }

//Dette er bra

//(t,ks) pattermatch on touples 

 
(t, ks) match {
    case ()
}


///
// p + (x->v)

//p[x->v]

// Rule 17
//Term: {(val x = e1) :: val e2}

// same as: 
{val e2} val = (val x = e1) :: val2

// Rulse 17, if I have a block. this is trying to extracto out the first x=e then val .... as a block 

def test(t: Term){
    t match {
        // Does t have a TermExp that has a BlockExp
        case TermExp(BlockExp(Val(x,e1):: vals, e2))=>{

        }
    }
}


List().reverce //works

recevere()//does not work 

//Rule 16
if(e1) e2 else e3 //GUARD.  ifK(e2,e3)::k need to evaluate this. if ("foo") -> get stuck 

// rule 22 if guard is ture and 23 is guard is false. 



