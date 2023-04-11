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



// GPT bud below 
    let rec spC(C: Command, P: Predicate): Predicate =
        let rec spGC(GC: GuardedCommand, P: Predicate): Predicate =
            match GC with
            | Guard(b, cmd) -> spC(cmd, BooleanOp(b, And, P))
            | Choice(GC1, GC2) -> BooleanOp(spGC(GC1, P), Or, spGC(GC2, P))

        match C with
        | Skip -> P

        | If(GuardedCommand) -> spGC(GuardedCommand, P)

        | Sep(C1, C2) -> spC(C2, spC(C1, P))
        | _ -> failwith "not implemented"
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


    let verification_conditions: List<Predicate> =
        [BooleanOp(spC(C,P), Implies, Q)] @ []

    // Let this line stay as it is.
    { verification_conditions = List.map serialize_predicate verification_conditions }

