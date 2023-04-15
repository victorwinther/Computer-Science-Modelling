module ProgramVerification

open System
open System.Linq.Expressions
open Predicate.AST

(*
    This defines the input and output for the program verification analysis.
    Please do not change the definitions below as they are needed for the
    validation and evaluation tools!
*)

type Input = unit

type Output =
    { verification_conditions: List<SerializedPredicate> }

let analysis (src: string) (input: Input) : Output =
    let (P, C, Q) =
        match Predicate.Parse.parse src with
        | Ok (AnnotatedCommand (P, C, Q)) -> P, C, Q
        | Error e ->
            failwith
                $"Failed to parse.\n\nDid you remember to surround your program with predicate blocks, like so?\n\n  {{ true }} skip {{ true }}\n\n{e}"

    // TODO: Remove these print statements
    Console.Error.WriteLine("P = {0}", P)
    Console.Error.WriteLine("C = {0}", C)
    Console.Error.WriteLine("Q = {0}", Q)
    
    let mutable counter = 0
    
    let rec spC(C: Command, P: Predicate): Predicate =
        match C with
        | Skip -> P
        | Assign(x,a) -> assignFunc(x, a, P)
        | If(GuardedCommand) -> spGC(GuardedCommand, P)
        | Do(inv, gc1) -> BooleanOp(inv, LOr, doneVC(gc1))
        | Sep(C1, C2) -> spC(C2, spC(C1, P))
        | _ -> failwith "something went wrong"
        
    and assignFunc(x:String, a:AExpr, P:Predicate): Predicate =
        let y_ = "_f" + counter.ToString()
        counter <- counter + 1
        let cP = BooleanOp(predicateSub(P, Variable(y_), Variable(x)), LAnd, RelationalOp(Variable(x), Eq, expressionSub(a, Variable(y_), Variable(x))))
       
        Exists(y_,cP)
        
    and spGC(GC: GuardedCommand, P: Predicate): Predicate =
        match GC with
        | Guard(b, cmd) -> spC(cmd, BooleanOp(b, LAnd, P))
        | Choice(GC1, GC2) -> BooleanOp(spGC(GC1, P), LOr, spGC(GC2, P))
    
    and predicateSub (p: Predicate, e: AExpr, x: AExpr) =
        match p with
        | Bool(b) -> Bool(b)
        | RelationalOp(e1, r, e2) ->
            RelationalOp(expressionSub(e1,e,x), r, expressionSub(e2,e,x))
        | Exists(i, p) -> Exists(i,predicateSub(p, e, x))
        | Forall(i, p) -> Forall(i,predicateSub(p, e, x))
        | Not(p) -> Not(predicateSub(p, e, x))
        | BooleanOp(p1, r, p2) -> BooleanOp(predicateSub(p1,e,x), r, predicateSub(p2,e,x))
    
    and expressionSub (e, e', y): AExpr =
        match e with
        | Number(n) -> Number(n)
        | Variable(x) -> if x.Equals(y.ToString()) then e' else Variable(x)
        | LogicalVariable(x) -> LogicalVariable(x)
        | Array(a, e1) -> Array(a, expressionSub(e1,e',y))
        | LogicalArray(a, e1) -> LogicalArray(a, expressionSub(e1, e', y))
        | Binary(e1, r, e2) -> Binary(expressionSub(e1, e', y), r, expressionSub(e2, e', y))
        | Function(Division(e1, e2)) -> Function(Division(expressionSub(e1, e', y), expressionSub(e2, e', y)))
        | Function(Min(e1,e2)) -> Function(Min(expressionSub(e1, e', y), expressionSub(e2, e', y)))
        | Function(Max(e1, e2)) -> Function(Max(expressionSub(e1, e', y), expressionSub(e2, e', y)))
        | Function(Count(s ,e1)) -> Function(Count(s, expressionSub(e1, e', y)))
        | Function(LogicalCount(s, e1)) -> Function(LogicalCount(s, expressionSub(e1, e', y)))
        | Function(Length(s)) -> Function(Length(s))
        | Function(LogicalLength(s)) -> Function(LogicalLength(s))
        | Function(Fac(e1)) -> Function(Fac(expressionSub(e1, e', y)))
        | Function(Fib(e1)) -> Function(Fib(expressionSub(e1, e', y)))
        
    and cmdVC (cmd: Command, R: Predicate) =
        match cmd with
        | Skip -> []
        | Assign(x,a) -> []
        | Sep(c1, c2) -> cmdVC(c1, R) @ cmdVC(c2, spC(c1, R))
        | If(gCmd) -> GCVC(gCmd, R)
        | Do(I, gCmd) -> [BooleanOp(R, Implies, I); BooleanOp(spGC(gCmd, I), Implies, I)] @ GCVC(gCmd, I)
        
        
    and GCVC (Gcmd: GuardedCommand, R: Predicate) =
        match Gcmd with
        | Guard(b, c1) -> cmdVC(c1, BooleanOp(b, LAnd, R))
        | Choice(gc1, gc2) -> GCVC(gc1, R) @ GCVC(gc2, R)
    
    and doneVC (gcmd: GuardedCommand) =
        match gcmd with
        | Guard(b, _) -> Not(b)
        | Choice(gc1, gc2) -> BooleanOp(doneVC(gc1), LAnd, doneVC(gc2))


    let verification_conditions: List<Predicate> =
        [BooleanOp(spC(C,P), Implies, Q)] @ cmdVC(C, P)

    // Let this line stay as it is.
    { verification_conditions = List.map serialize_predicate verification_conditions }

