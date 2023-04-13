module ProgramVerification

open System
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


    let rec doneGC(GC: GuardedCommand) : Predicate =
        match GC with
        | Guard(b,c) -> Not(b)
        | Choice(gc1, gc2) -> BooleanOp(doneGC(gc1), And, doneGC(gc2))

    let mutable counter = 0

    let assignFunc(x:String,a:AExpr,P:Predicate):Predicate =
        let cP = BooleanOp(P,LAnd,RelationalOp(Variable(x),Eq,a))
        let newWar = "_"+x

        Exists(newWar,cP)

    let rec spC(C: Command, P: Predicate): Predicate =
        let rec spGC(GC: GuardedCommand, P: Predicate) : Predicate =
            match GC with
            | Guard(b, c) -> spC(c, BooleanOp(b, LAnd, P))
            | Choice(gc1, gc2) -> BooleanOp(spGC(gc1, P), Or, spGC(gc2, P))
            | _ -> failwith "not yet"
        
        match C with
        | Skip -> P
        | Assign(x,a) -> assignFunc(x, a, P)
        | Sep(c1,c2) -> spC(c2, spC(c1, P))
        | If(gc) -> spGC(gc, P)
        | Do(I, gc) -> BooleanOp(I, And, doneGC(gc))
        | _ -> failwith "not implemented"

    let rec vcC (C: Command, R: Predicate) =
        let rec vcGC(GC: GuardedCommand, R: Predicate) =
            match GC with
            | Guard(b,c) -> vcC(c,BooleanOp(b,And,R))
            | Choice(gc1,gc2) -> vcGC(gc1,R) @ vcGC(gc2,R)
            | _ -> failwith "not yet"

        match C with
        | Skip -> []
        | Assign(x,a)-> []
        | Sep(c1,c2) -> vcC(c1,R) @ vcC(c2,spC(c1,R))
        | If(gc) -> vcGC(gc,R)
        //| Do(I,gc) -> [R, Implies, I, (spC((gc),I)), Implies, I] @ vcGC(gc,I)
        | _ -> failwith "not yet"
    
    let verification_conditions : List<Predicate> =
        [BooleanOp(spC(C,P), Implies, Q)] @ vcC(C,P)

    // Let this line stay as it is.
    { verification_conditions = List.map serialize_predicate verification_conditions }

(*
    let rec spC(C: Command, P: Predicate): Predicate =
        match C with
        | Skip -> P
    
        // test linje til inspectify:
        // {true} if true -> skip fi {true}
        | If(Guard(b: BExpr ,cmd: Command)) -> spC((cmd), BooleanOp(b, And, P))

        // {true} if (3 > -53) ->   skip [] (3 = 3) ->    skip fi {false} - inspectify test
        | If (Choice(Guard(b1, gc1), Guard(b2, gc2))) -> BooleanOp(spC(gc1, P), Or, spC(gc2, P))

        // assign virker ikke - ved ikke lige hvordan man skal skrive det :))
        //| Assign(str, aexpr) -> BooleanOp(P, And, BooleanOp(str, Implies, aexpr))


        | Sep(C1, C2) -> spC(C2, spC(C1, P))
        | _ -> failwith "not implemented"
*)

