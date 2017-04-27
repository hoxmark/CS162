def head(list) = 
list match {
    case Cons(cell) => cell._1
    case Nil(x) => 1*"Moo" //intentonally
}


