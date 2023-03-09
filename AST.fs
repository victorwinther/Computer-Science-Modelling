// This file implements a module where we define a data type "expr"
// to store represent arithmetic expressions
module AST

type Command =
    | Skip
    | Sequence of Command * Command

//C  ::=  x := a  |  A[a] := a  |  skip  |  C ; C  |  if GC fi  |  do GC od

//GC ::=  b -> C  |  GC [] GC

//a  ::=  n  |  x  |  A[a]  |  a + a  |  a - a  |  a * a  |  a / a  |  - a  |  a ^ a  |  (a)

//b  ::=  true  |  false  |  b & b  |  b | b  |  b && b  |  b || b  |  ! b
//     |  a = a  |  a != a  |  a > a  |  a >= a  |  a < a  |  a <= a  |  (b)


type expr =
    | Num of float
    | TimesExpr of (expr * expr)
    | DivExpr of (expr * expr)
    | PlusExpr of (expr * expr)
    | MinusExpr of (expr * expr)
    | PowExpr of (expr * expr)
    | UPlusExpr of (expr)
    | UMinusExpr of (expr)
