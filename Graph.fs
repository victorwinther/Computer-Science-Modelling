module Graph

open Types
open Parse
open FSharp.Text.Lexing
open System
open AST


(*
    This defines the input and output for graphs. Please do not
    change the definitions below as they are needed for the validation and
    evaluation tools!
*)

type Input = { determinism: Determinism }

type Output = { dot: string }

type Node = I of int | F of string

type Label = Command

type Edge = {
    source : Node;
    label : Label;
    target : Node;
}

let mutable f = 0

let rec edges (ast: Command, qS: Node, qF: Node) : List<Edge> =
    match ast with 
    | DeclareVar(_) -> [{source = qS; label = ast ; target = qF}]
    | DeclareArr(_) -> [{source = qS; label = ast ; target = qF}]
    | Skip ->  [{source = qS; label = ast ; target = qF}]
    | Sequence(c1, c2) -> (f<-f+1); edges(c1, qS, I(f)) @ edges(c2, I(f), qF)
    //| Sequence(_) ->  [{source = qS; label = ast ; target = qF}]
    | _ -> List.empty

//edges(c1, qS,qF) + ";" + edges(c2, qS, qF)

let ast2pg (ast: Command) : List<Edge> =
    edges(ast, F("q0") , F("qF"))

let label2dot(l:Label) : string =
    "[label = " + prettyPrint(C(l)) 0 + "]"
    
let node2string (n: Node) : string =
    match n with
    | I(i) -> string i
    | F(s) -> s


let edge2dot (e: Edge) : string =
    node2string(e.source) + " -> " + node2string(e.target) + " " + label2dot(e.label) + " ;"


let rec edges2dot (pg : List<Edge> ) : string = 
    match pg with
    | [] -> ""
    | e::pg -> edge2dot(e) + edges2dot(pg)

let pg2dot (pg : List<Edge> ) : string = 
    "digraph program_graph {rankdir=LR; " +
    edges2dot(pg) + " }"

let analysis (src: string) (input: Input) : Output =
    match parse Parser.startGCL src with
        | Ok ast ->
            let pg = ast2pg(ast)
            // Console.Error.WriteLine("> {0}",pg)
            let dotstring = pg2dot(pg)
            { dot = dotstring }

        | Error e -> {dot = ""}


// dotnet run graph "skip" "{\"determinism\":{\"Case\":\"Deterministic\"}}" 
