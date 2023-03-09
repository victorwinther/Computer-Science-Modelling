// This file implements a module where we define a data type "expr"
// to store represent arithmetic expressions
module AST

//a  ::=  n  |  x  |  A[a]  |  a + a  |  a - a  |  a * a  |  a / a  |  - a  |  a ^ a  |  (a)    
type expr =
    | Num of float
    | Variable of string
    | ArrayVal of (string * expr)
    | TimesExpr of (expr * expr)
    | DivExpr of (expr * expr)
    | PlusExpr of (expr * expr)
    | MinusExpr of (expr * expr)
    | PowExpr of (expr * expr)
    | UPlusExpr of (expr)
    | UMinusExpr of (expr)
    | Par of (expr)

//b  ::=  true  |  false  |  b & b  |  b | b  |  b && b  |  b|| b  |  ! b
//     |  a = a  |  a != a  |  a > a  |  a >= a  |  a < a  |  a <= a  |  (b)
type BoolExpr =
    | True
    | False
    | BoolAnd of (BoolExpr * BoolExpr)
    | BoolOr of (BoolExpr * BoolExpr)
    | BoolAndAnd of (BoolExpr * BoolExpr)
    | BoolOrOr of (BoolExpr * BoolExpr)
    | BoolNot of (BoolExpr)
    | BoolEqual of (expr * expr)
    | BoolNotEqual of (expr * expr)
    | BoolGreater of (expr * expr)
    | BoolGreaterOrEqual of (expr * expr)
    | BoolLess of (expr * expr)
    | BoolLessOrEqual of (expr * expr)
    | Par of (BoolExpr)

//C  ::=  x := a  |  A[a] := a  |  skip  |  C ; C  |  if GC fi  |  do GC od
type Command =
    | DeclareVar of (string * expr)
    | DeclareArr of (string * expr * expr)
    | Skip
    | Sequence of (Command * Command)
    | If of GuardedCommand
    | Do of GuardedCommand
    
//GC ::=  b -> C  |  GC [] GC
and GuardedCommand =
    | Condition of (BoolExpr * Command)
    | Else of (GuardedCommand * GuardedCommand)

type AST = 
    | S of string
    | A of expr
    | B of BoolExpr
    | C of Command
    | GC of GuardedCommand



    