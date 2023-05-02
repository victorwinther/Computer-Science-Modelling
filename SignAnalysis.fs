module SignAnalysis

open Types
open AST

(*
     This defines the input and output of the sign analysis. Please do not
    change the definitions below as they are needed for the validation and
    evaluation tools!
*)

type Sign =
    | Negative
    | Zero
    | Positive

type SignAssignment =
    { variables: Map<string, Sign>
      arrays: Map<string, Set<Sign>> }

type Input =
    { determinism: Determinism
      assignment: SignAssignment }

type Output =
    { initial_node: string
      final_node: string
      nodes: Map<string, Set<SignAssignment>> }


let analysis (src: string) (input: Input) : Output =
    failwith "Sign analysis not yet implemented" // TODO: start here
    
    
    
    let rec scemanticA (e: expr, mem1: SignAssignment, mem2: SignAssignment) =
        match e with
        | Num(e) -> if e > 0 then set [Positive] else if e < 0 then set [Negative] else set [Zero]
        | Variable(e) -> set [mem1.variables[e]]
        | PlusExpr(a1, a2) -> 
    
    
    
    and passTheAux(s1, op, s2) =
        let calc = match op with
        | "+" -> Set.map (fun x -> Set.map (fun y -> ))
    
    
    
    
    
    
    
    
    
    
    
