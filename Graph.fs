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

type Node = string
type Label = Command

type Edge = {
    source : Node;
    label : Label;
    target : Node;
}
let edges (ast: Command, qS: Node, qF: Node) : List<Edge> =
    match ast with 
    | Skip ->  [{source = qS; label = ast ; target = qF}]
    | Sequence(c1, c2) ->  List.empty
    | _ -> List.empty

let ast2pg (ast: Command) : List<Edge> =
    edges(ast, "q0" , "qF")

let label2dot(l:Label) : string =
    "[label = " + prettyPrint(C(l)) 0 + "]"

let edge2dot (e: Edge) : string =
    e.source + " -> " + e.target + " " + label2dot(e.label) + " ;"

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
