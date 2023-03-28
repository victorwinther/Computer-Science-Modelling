module Interpreter

open Types
open Parse
open FSharp.Text.Lexing
open System
open AST
open Graph

type InterpreterMemory =
    { variables: Map<string, int>
      arrays: Map<string, List<int>> }

type Input =
    { determinism: Determinism
      assignment: InterpreterMemory
      trace_length: int }

type Node = Graph.Node

type TerminationState =
    | Running
    | Stuck
    | Terminated

type Configuration<'node> =
    { node: 'node
      memory: InterpreterMemory 
      }

type Output =
   { execution_sequence: List<Configuration<string>>
     final: TerminationState }

//let stringifyNode (internalNode: Node) : string =
//   match internalNode with
//   | I(-1) -> "qF"
//   | _ -> nodeToString(internalNode)

let prepareConfiguration (c: Configuration<Node>) : Configuration<string> =
   { node = nodeToString c.node
     memory = c.memory }

let rec insertInList x l i=
    match l,i with
    | l::list, 0 -> x::list
    | l::list,_ -> l::insertInList x list (i-1)
    | [],_ -> []
let rec listCreator (list:List<int>) (length:int) : List<int>=
    match list,length with
    | _,0 -> []
    | l,length -> (listCreator (0::l) (length-1))
let arrayGetter x index value (m:InterpreterMemory)=
    match m.arrays.ContainsKey(x) with
    | true -> let check = m.arrays.[x]
              if check.Length > index then m.arrays.Add(x,insertInList value check index) else  m.arrays.Add(x,insertInList value (check@listCreator [] (index-check.Length+1))index)
    | false -> m.arrays.Add(x,insertInList value (listCreator [] (index+1)) index)

     

let rec ArithmeticSemantic (lab: expr) (m: InterpreterMemory) =
    match lab with
    | Num(n) -> Some(n|>int)
    | Variable(x) -> if m.variables.ContainsKey x  then Some(m.variables.[x]) else None
    | ArrayVal(s,x) -> 
                                    if m.arrays.ContainsKey s  then (if m.arrays.[s].Length>(ArithmeticSemantic x m).Value then Some(m.arrays.[s].[(ArithmeticSemantic x m).Value]) else None) 
                                    else None
    | TimesExpr(a1,a2) ->
                       let a1m = ArithmeticSemantic a1 m 
                       let a2m = ArithmeticSemantic a2 m
                       if a1m.IsSome && a2m.IsSome then Some(a1m.Value*a2m.Value) else None
    | DivExpr(a1,a2) ->
                     let a1m = ArithmeticSemantic a1 m 
                     let a2m = ArithmeticSemantic a2 m
                     if a1m.IsSome && a2m.IsSome then Some(a1m.Value/a2m.Value) else None
    | PlusExpr(a1,a2) ->
                    let a1m = ArithmeticSemantic a1 m 
                    let a2m = ArithmeticSemantic a2 m
                    if a1m.IsSome && a2m.IsSome then Some(a1m.Value+a2m.Value) else None
    | MinusExpr(a1,a2) ->
                       let a1m = ArithmeticSemantic a1 m 
                       let a2m = ArithmeticSemantic a2 m
                       if a1m.IsSome && a2m.IsSome then Some(a1m.Value-a2m.Value) else None
    | PowExpr(a1,a2) ->
              let a1m = ArithmeticSemantic a1 m 
              let a2m = ArithmeticSemantic a2 m
              if a1m.IsSome && a2m.IsSome && (a2m.Value>=0) then Some(((a1m.Value|>float)**(a2m.Value|>float))|>int) else None
    | UMinusExpr(a1) ->
                      let a1m = ArithmeticSemantic a1 m
                      if a1m.IsSome then Some(-1*a1m.Value) else None
    
and BooleanSemantic  (lab: BoolExpr) (m : InterpreterMemory) =
    match lab with
    | True -> Some(true)
    | False -> Some(false)
    | BoolAnd(b1,b2) ->
                    let b1m = BooleanSemantic b1 m
                    let b2m = BooleanSemantic b2 m
                    if b1m.IsNone then None else (if b1m.Value then Some(b2m.Value) else Some(false)) 
    | BoolOr(b1,b2) ->
                    let b1m = BooleanSemantic b1 m
                    let b2m = BooleanSemantic b2 m
                    if b1m.IsNone then None else (if b1m.Value then Some(b1m.Value) else Some(b2m.Value)) 
    | BoolAndAnd(b1,b2) ->
                       let b1m = BooleanSemantic b1 m
                       let b2m = BooleanSemantic b2 m
                       if b1m.IsNone || b2m.IsNone then None else (if b1m.Value && b2m.Value then Some(true) else Some(false))
    | BoolOrOr(b1,b2) ->
                      let b1m = (BooleanSemantic b1 m)
                      let b2m = BooleanSemantic b2 m
                      if b1m.Value = true || b2m.Value = true then Some(true) else (if b1m.Value = false && b2m.Value = false then Some(false) else None)
    | BoolNot(b1) ->
                 let b1m = BooleanSemantic b1 m
                 if b1m.IsNone then None else if b1m.Value = false then Some(true) else Some(false)
    | BoolEqual(a1,a2) ->
                       let a1m = ArithmeticSemantic a1 m
                       let a2m = ArithmeticSemantic a2 m
                       if a1m.IsSome && a2m.IsSome then (if (ArithmeticSemantic a1 m).Value = (ArithmeticSemantic a2 m).Value then Some(true) else Some(false)) else None
    | BoolNotEqual(a1,a2) ->
                       let a1m = ArithmeticSemantic a1 m
                       let a2m = ArithmeticSemantic a2 m
                       if a1m.IsSome && a2m.IsSome then (if (ArithmeticSemantic a1 m).Value <> (ArithmeticSemantic a2 m).Value then Some(true) else Some(false)) else None
    | BoolGreater(a1,a2) ->
                         let a1m = ArithmeticSemantic a1 m
                         let a2m = ArithmeticSemantic a2 m
                         if a1m.IsSome && a2m.IsSome then (if (ArithmeticSemantic a1 m).Value > (ArithmeticSemantic a2 m).Value then Some(true) else Some(false)) else None  
    | BoolGreaterOrEqual(a1,a2) ->
                                 let a1m = ArithmeticSemantic a1 m
                                 let a2m = ArithmeticSemantic a2 m
                                 if a1m.IsSome && a2m.IsSome then (if (ArithmeticSemantic a1 m).Value >= (ArithmeticSemantic a2 m).Value then Some(true) else Some(false)) else None 
    | BoolLess(a1,a2) ->
                     let a1m = ArithmeticSemantic a1 m
                     let a2m = ArithmeticSemantic a2 m
                     if a1m.IsSome && a2m.IsSome then (if (ArithmeticSemantic a1 m).Value < (ArithmeticSemantic a2 m).Value then Some(true) else Some(false)) else None 
    | BoolLessOrEqual(a1,a2) ->
                             let a1m = ArithmeticSemantic a1 m
                             let a2m = ArithmeticSemantic a2 m
                             if a1m.IsSome && a2m.IsSome then (if (ArithmeticSemantic a1 m).Value <= (ArithmeticSemantic a2 m).Value then Some(true) else Some(false)) else None 
                    
and Semantic(lab: Label, m: InterpreterMemory) : Option<'InterpreterMemory> =
    match lab with
    | C(ast) -> match ast with
                | Skip -> Some(m)
                | DeclareVar(x, a) ->
                                   let (a1: int option) = (ArithmeticSemantic a m)
                                   if a1.IsSome then Some({variables = m.variables.Add(x,a1.Value); arrays =m.arrays}) else None
                | DeclareArr(x,a) -> 
                                    let index = match x with
                                                                    | ArrayVal(n,e) -> e
                                                                    | _ -> a
                                    let ArrayName = match x with 
                                                                    | ArrayVal(n,e) -> n
                                                                    | _ -> "Faliure"
                                    let v = ArithmeticSemantic a m
                                    let A = ArithmeticSemantic x m
                                    if (v.IsSome && (ArithmeticSemantic index m).IsSome) then Some({variables = m.variables; arrays = arrayGetter ArrayName (ArithmeticSemantic index m).Value v.Value m}) else None                  

    | B(ast) -> if (BooleanSemantic ast m).Value then Some(m) else None
    | _ -> None

//def 1.11
let rec ExecutionSteps(pg: ProgramGraph, q: Node, m: InterpreterMemory) : List<Configuration<'node>> =
    match pg with
    | e::pg2 ->
        let lab = e.label
        let t = e.target
        if not (e.source.Equals(q)) then
            ExecutionSteps(pg2, q, m)
        else
            let memPrime = Semantic(lab, m)
            if memPrime.IsSome then [{node=t; memory=memPrime.Value}]@ExecutionSteps(pg2, q, m)
            else
                ExecutionSteps(pg2, q, m)
    | [] -> []
    
// def 1.13    
let rec ExecutionSequence (pg: ProgramGraph, q: Node, m: InterpreterMemory, tl: int, stateCount: int) =
    let nextStates = ExecutionSteps(pg, q, m)
    match tl, nextStates with
    | 0, _ -> [ {node=q; memory=m} ], 0
    | _, [] -> ([ {node=q; memory=m} ], if q.Equals(F("qF")) then 2 else 1)
    | _, config::confList -> let iniEdge = [ {node=q; memory = m} ]
                             let nextEdge, newCount = ExecutionSequence(pg, config.node, config.memory, tl-1, stateCount)
                             (iniEdge@nextEdge, newCount)

let analysis (src: string) (input: Input) : Output =
    match parse Parser.startGCL src with
    | Ok ast ->
        let pg = astToProgramGraph(C(ast))
        let e, stateCounter = ExecutionSequence(pg, F("q0"), input.assignment, input.trace_length, 0)
        let execution_sequence: List<Configuration<Node>> = e
        let final: TerminationState = if stateCounter = 1 then Stuck else if stateCounter = 2 then Terminated else Running
        { execution_sequence = List.map prepareConfiguration execution_sequence
          final = final }
    | Error error -> failwith "Analysis error"
   