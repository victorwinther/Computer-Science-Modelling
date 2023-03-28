module Graph

open Types
open Parse
open FSharp.Text.Lexing
open System
open AST

type Input = { determinism: Determinism }

type Output = { dot: string }

type Node = I of int | F of string

type Label = AST

type Edge = {
    source : Node;
    label : AST;
    target : Node;
}
type ProgramGraph = Edge list

type ProgramGraph = Edge List

let mutable i = 0

let rec edges (ast: AST, qS: Node, qF: Node) : ProgramGraph =
    
    let rec commandEdge cmd =
        match cmd with
        | Sequence(c1, c2) -> (i<-i+1); edges(C(c1), qS, I(i)) @ edges(C(c2), I(i), qF)
        | If(gcmd) -> edges(GC(gcmd), qS, qF)
        | Do(gcmd) -> edges(GC(gcmd), qS, qF)@[{source = qS;label = GC(gcmd);target =qF}]
        | _ -> [{source = qS; label = ast ; target = qF}]
    
    let rec GuardedCommandEdge Gcmd =
        match Gcmd with
        | Else(gc1, gc2) -> edges (GC(gc1), qS, qF) @ edges (GC(gc2), qS, qF)
        | Condition(b, cmd) -> i<-i+1; {source = qS; label = B(b); target = I(i)}::edges(C(cmd), qF, I(i))
    
    match ast with 
    | C(command) -> commandEdge(command)
    | GC(Gcommand) -> GuardedCommandEdge(Gcommand)
    | _ -> List.empty

let astToProgramGraph (ast: AST) : ProgramGraph =
    edges(ast, F("q0") , F("qF"))

let labelToString(label :Label) : string =
    sprintf "[label=%A]" (prettyPrint label 0)
    //"[label = " + prettyPrint(C(l)) 0 + "]"
    
let nodeToString (n: Node) : string =
    match n with
    | I(i) -> string i
    | F(s) -> s

let edgeToString (e: Edge) : string =
    nodeToString(e.source) + " -> " + nodeToString(e.target) + " " + labelToString(e.label) + " ;"


let rec edgesToString (pg : List<Edge> ) : string = 
    match pg with
    | [] -> ""
    | e::pg -> sprintf "%s " (edgeToString e) + edgesToString pg
    //| e::pg -> edgeToString(e) + edgesToString(pg)

let programToString (pg : List<Edge> ) : string = 
    "digraph program_graph {rankdir=LR; " + (edgesToString pg) + " }"

let analysis (src: string) (input: Input) : Output =
    match parse Parser.startGCL src with
        | Ok ast ->
            let pg = astToProgramGraph(C(ast))
            let dotstring = programToString(pg)
            { dot = dotstring }

        | Error e -> {dot = ""}
