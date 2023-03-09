module Parse

open System.Linq.Expressions
open FSharp.Text.Lexing
open System
open AST

exception ParseError of Position * string * Exception

let parse parser src =
    let lexbuf = LexBuffer<char>.FromString src

    let parser = parser Lexer.tokenize

    try
        Ok(parser lexbuf)
    with
    | e ->
        let pos = lexbuf.EndPos
        let line = pos.Line
        let column = pos.Column
        let message = e.Message
        let lastToken = new String(lexbuf.Lexeme)
        eprintf "Parse failed at line %d, column %d:\n" line column
        eprintf "Last token: %s" lastToken
        eprintf "\n"
        Error(ParseError(pos, lastToken, e))

let newline = "\n"
let indent i = (String.replicate i " ")

let rec prettyPrint (ast:AST) i = 

    let print_cmd cmd =
        match cmd with
        | DeclareVar(s,a) -> ":=" + newline + (prettyPrint (S(s)) (i+1)) + newline + (prettyPrint (A(a)) (i+1))
        | DeclareArr(s,a,b) ->  "[]:=" + newline +  (prettyPrint (S(s)) (i+1)) + newline + (prettyPrint (A(a)) (i+1)) + newline + (prettyPrint (A(b)) (i+1))
        | Skip ->  "Skip"
        | Sequence(c1,c2) ->  ";" + newline + (prettyPrint (C(c1)) (i+1)) + newline + (prettyPrint (C(c2)) (i+1))
        | If(guardedcmd) -> "if x fi" + newline + (prettyPrint (GC(guardedcmd)) (i+1))
        | Do(guardedcmd) -> "do x od" + newline + (prettyPrint (GC(guardedcmd)) (i+1))


    let print_ari ari =
        match ari with
        | Num(f) ->  string(f)
        | Variable(s) ->  s
        | TimesExpr(a,b) -> "*" + newline + (prettyPrint (A(a)) (i+1)) + newline + (prettyPrint (A(b)) (i+1))
        | DivExpr(a,b) -> "/" + newline + (prettyPrint (A(a)) (i+1)) + newline + (prettyPrint (A(b)) (i+1))
        | PlusExpr(a,b) -> "+" + newline + (prettyPrint (A(a)) (i+1)) + newline + (prettyPrint (A(b)) (i+1))
        | MinusExpr(a,b) -> "- "+ newline + (prettyPrint (A(a)) (i+1)) + newline + (prettyPrint (A(b)) (i+1))
        | PowExpr(a,b) -> "^" + newline + (prettyPrint (A(a)) (i+1)) + newline + (prettyPrint (A(b)) (i+1))
        | UMinusExpr(a) -> "-" + newline + (prettyPrint (A(a)) (i+1))
        | UPlusExpr(a) -> "+" + newline + (prettyPrint (A(a)) (i+1))
        | ArrayVal(s,a) -> "[]" + newline + (prettyPrint (S(s)) (i+1)) + newline + (prettyPrint (A(a)) (i+1))
        | expr.Par(a) -> (prettyPrint (A(a)) (i))


    let print_bools bools = 
        match bools with
        | True -> "true"
        | False -> "false"
        | BoolAnd(a,b) -> "&" + newline + (prettyPrint (B(a)) (i+1)) + newline + (prettyPrint (B(b)) (i+1))
        | BoolOr(a,b) -> "|" + newline + (prettyPrint (B(a)) (i+1)) + newline + (prettyPrint (B(b)) (i+1))
        | BoolAndAnd(a,b) -> "&&" + newline + (prettyPrint (B(a)) (i+1)) + newline + (prettyPrint (B(b)) (i+1))
        | BoolOrOr(a,b) -> "||" + newline + (prettyPrint (B(a)) (i+1)) + newline + (prettyPrint (B(b)) (i+1))
        | BoolNot(a) -> "!" + newline + (prettyPrint (B(a)) (i+1))
        | BoolEqual(a,b) -> "=" + newline + (prettyPrint (A(a)) (i+1)) + newline + (prettyPrint (A(b)) (i+1))
        | BoolNotEqual(a,b) -> "!=" + newline + (prettyPrint (A(a)) (i+1)) + newline + (prettyPrint (A(b)) (i+1))
        | BoolGreater(a,b) -> ">" + newline + (prettyPrint (A(a)) (i+1)) + newline + (prettyPrint (A(b)) (i+1))
        | BoolGreaterOrEqual(a,b) -> ">=" + newline + (prettyPrint (A(a)) (i+1)) + newline + (prettyPrint (A(b)) (i+1))
        | BoolLess(a,b) -> "<" + newline + (prettyPrint (A(a)) (i+1)) + newline + (prettyPrint (A(b)) (i+1))
        | BoolLessOrEqual(a,b) -> "<=" + newline + (prettyPrint (A(a)) (i+1)) + newline + (prettyPrint (A(b)) (i+1))
        | Par(a) -> (prettyPrint (B(a)) (i))

    let print_gcmd gcmd = 
        match gcmd with
        | Condition(b,c) -> "->" + newline + (prettyPrint (B(b)) (i+1)) + newline + (prettyPrint (C(c)) (i+1))
        | Else(a,b) -> "[]" + newline + (prettyPrint (GC(a)) (i+1)) + newline + (prettyPrint (GC(b)) (i+1))
        
    match ast with
        | S(s) -> (indent i) + s
        | A(a) -> (indent i) + print_ari a
        | B(b) -> (indent i) + print_bools b
        | C(c) -> (indent i) + print_cmd c
        | GC(gc) -> (indent i) + print_gcmd gc
    
//let rec prettyPrint ast =
//   match ast with
//   | DeclareVar(s, a) -> s + ":=" + a
//   | Skip -> "skip"
//   | Sequence(c1,c2) -> prettyPrint(c1) + "\n" + prettyPrint(c2)

let analysis (src: string) : string =
    match parse Parser.startGCL src with
        | Ok ast ->
            Console.Error.WriteLine("> {0}", ast)
            prettyPrint ast 0
        | Error e -> "Parse error: {0}"