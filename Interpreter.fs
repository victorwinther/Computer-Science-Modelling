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

let stringifyNode (internalNode: Node) : string =
   match internalNode with
   | I(-1) -> "qF"
   | _ -> "q" + internalNode.ToString()

let prepareConfiguration (c: Configuration<Node>) : Configuration<string> =
   { node = stringifyNode c.node
     memory = c.memory }

let Semantic(lab: Label, m: InterpreterMemory) : Option<'InterpreterMemory> =
    match lab with
    | C(ast) -> match ast with
                Skip -> Some(m)
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
    | _, [] -> ([ {node=q; memory=m} ], if q = I(-1) then 2 else 1)
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
   
    
