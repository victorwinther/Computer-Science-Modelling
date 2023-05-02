module Security

open System
open System.Linq.Expressions
open AST

type Flow = { from: string; into: string }
let flow a b : Flow = { from = a; into = b }

type Classification =
    { variables: Map<string, string>
      arrays: Map<string, string> }

type Input =
    { lattice: Flow list
      classification: Classification }

type Output =
    { actual: Flow list
      allowed: Flow list
      violations: Flow list }


let rec Sec (cmd: Command, X) : List<Flow> =
    match cmd with
    | DeclareVar(x,a) -> nestedList(Set.union (X) (fvA(a, set [])), set [x])
    | DeclareArr(A0, a1, a2) -> nestedList(Set.union (X) (Set.union (fvA(a1, set [])) (fvA(a2, set []))), set [A0])
    | Skip -> []
    | Sequence(c1, c2) -> Set.toList (Set.union (Set.ofList(Sec(c1, X))) (Set.ofList(Sec(c2, X))))
    | If(gc) -> gSec(gc, X)
    | Do(gc) -> gSec(gc, X)

and nestedList (s1, s2) = Set.toList(Set.unionMany(Set.map(fun x -> (Set.map (fun y -> flow x y) s2)) s1 ) )

and gSec(gC: GuardedCommand, X: Set<string>) : List<Flow> =
    match gC with
    | Condition(b, c1) -> Sec(c1, (Set.union (X) (fvB(b, set []))))
    | Else(gc1,gc2) -> Set.toList (Set.union (Set.ofList(gSec(gc1,X))) (Set.ofList (gSec(gc2, (Set.union (X) (implicitDeps(gc1)) ) ))))
    
    
and implicitDeps (e) =
    match e with
    | Condition(b, cmd) -> fvB(b, set [])
    | Else(gc1, gc2 ) -> Set.union (implicitDeps(gc1)) (implicitDeps(gc2)) 

and fvB (e: BoolExpr, s: Set<string>) =
    match e with
    | True -> set []
    | False -> set []
    | BoolAnd(b1, b2) -> Set.union (fvB(b1, s)) (fvB(b2, s))
    | BoolOr(b1, b2) -> Set.union (fvB(b1, s)) (fvB(b2, s))
    | BoolAndAnd(b1, b2) -> Set.union (fvB(b1, s)) (fvB(b2, s))
    | BoolOrOr(b1, b2) -> Set.union (fvB(b1, s)) (fvB(b2, s))
    | BoolNot(b1)-> fvB(b1, s)
    | BoolEqual(b1, b2) -> Set.union (fvA(b1, s)) (fvA(b2, s))
    | BoolNotEqual(b1, b2) -> Set.union (fvA(b1, s)) (fvA(b2, s))
    | BoolGreater(b1, b2) -> Set.union (fvA(b1, s)) (fvA(b2, s))
    | BoolGreaterOrEqual(b1, b2) -> Set.union (fvA(b1, s)) (fvA(b2, s))
    | BoolLess(b1, b2) -> Set.union (fvA(b1, s)) (fvA(b2, s))
    | BoolLessOrEqual(b1, b2) -> Set.union (fvA(b1, s)) (fvA(b2, s))
    | Par(b1) -> fvB(b1, s)
                            
and fvA (e: expr, s: Set<string>) : Set<string> =
    match e with
    | Num(a) -> Set []
    | Variable(a) -> Set.add a s
    | ArrayVal(a, x) -> Set.add a s
    | TimesExpr(a1, a2) -> Set.union (fvA(a1, s)) (fvA(a2, s))
    | DivExpr(a1, a2) -> Set.union (fvA(a1, s)) (fvA(a2, s))
    | PlusExpr(a1, a2) -> Set.union (fvA(a1, s)) (fvA(a2, s))
    | MinusExpr(a1, a2) -> Set.union (fvA(a1, s)) (fvA(a2, s))
    | PowExpr(a1, a2) -> Set.union (fvA(a1, s)) (fvA(a2, s))
    | UPlusExpr(a1) -> (fvA(a1, s))
    | UMinusExpr(a1) -> (fvA(a1, s))
    

let analysis (src: string) (input: Input) : Output =
    match parse Parser.startGCL (scr) with
    | Ok ast -> {actual:Sec(ast,set [] ) ; allowed [] ; violations= []}
    | Error error -> failwith "Analysis error"
