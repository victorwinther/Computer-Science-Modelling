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
        eprintf "Message: %s" message
        eprintf "\n"
        Error(ParseError(pos, lastToken, e))

let spacer i = (String.replicate i " ")
let newline = "\n"

let rec prettyPrint (ast:AST) i = 

    let printCommand command =
        match command with
        | DeclareVar(s,a) -> " \" " + (prettyPrint (S(s)) (i+1)) + " :=" + (prettyPrint (A(a)) (i+1)) + " \" "
        | DeclareArr(s,a,b) ->  "[]:=" + "\n" +  (prettyPrint (S(s)) (i+1)) + "\n" + (prettyPrint (A(a)) (i+1)) + "\n" + (prettyPrint (A(b)) (i+1))
        | Skip ->  "Skip"
        | Sequence(c1,c2) -> (prettyPrint (C(c1)) (i+1)) + ";" + (prettyPrint (C(c2)) (i+1)) 
        | If(guardedcmd) -> "if x fi" + "\n" + (prettyPrint (GC(guardedcmd)) (i+1))
        | Do(guardedcmd) -> "do x od" + "\n" + (prettyPrint (GC(guardedcmd)) (i+1))


    let printExpr expr =
        match expr with
        | Num(f) ->  string(f)
        | Variable(s) ->  s
        | TimesExpr(a,b) -> "*" + "\n" + (prettyPrint (A(a)) (i+1)) + "\n" + (prettyPrint (A(b)) (i+1))
        | DivExpr(a,b) -> "/" + "\n" + (prettyPrint (A(a)) (i+1)) + "\n" + (prettyPrint (A(b)) (i+1))
        | PlusExpr(a,b) -> "+" + "\n" + (prettyPrint (A(a)) (i+1)) + "\n" + (prettyPrint (A(b)) (i+1))
        | MinusExpr(a,b) -> "- "+ "\n" + (prettyPrint (A(a)) (i+1)) + "\n" + (prettyPrint (A(b)) (i+1))
        | PowExpr(a,b) -> "^" + "\n" + (prettyPrint (A(a)) (i+1)) + "\n" + (prettyPrint (A(b)) (i+1))
        | UMinusExpr(a) -> "-" + "\n" + (prettyPrint (A(a)) (i+1))
        | UPlusExpr(a) -> "+" + "\n" + (prettyPrint (A(a)) (i+1))
        | ArrayVal(s,a) -> "[]" + "\n" + (prettyPrint (S(s)) (i+1)) + "\n" + (prettyPrint (A(a)) (i+1))
        | expr.Par(a) -> (prettyPrint (A(a)) i)


    let printBool bool = 
        match bool with
        | True -> "true"
        | False -> "false"
        | BoolAnd(a,b) -> "&" + "\n" + (prettyPrint (B(a)) (i+1)) + "\n" + (prettyPrint (B(b)) (i+1))
        | BoolOr(a,b) -> "|" + "\n" + (prettyPrint (B(a)) (i+1)) + "\n" + (prettyPrint (B(b)) (i+1))
        | BoolAndAnd(a,b) -> "&&" + "\n" + (prettyPrint (B(a)) (i+1)) + "\n" + (prettyPrint (B(b)) (i+1))
        | BoolOrOr(a,b) -> "||" + "\n" + (prettyPrint (B(a)) (i+1)) + "\n" + (prettyPrint (B(b)) (i+1))
        | BoolNot(a) -> "!" + "\n" + (prettyPrint (B(a)) (i+1))
        | BoolEqual(a,b) -> "=" + "\n" + (prettyPrint (A(a)) (i+1)) + "\n" + (prettyPrint (A(b)) (i+1))
        | BoolNotEqual(a,b) -> "!=" + "\n" + (prettyPrint (A(a)) (i+1)) + "\n" + (prettyPrint (A(b)) (i+1))
        | BoolGreater(a,b) -> ">" + "\n" + (prettyPrint (A(a)) (i+1)) + "\n" + (prettyPrint (A(b)) (i+1))
        | BoolGreaterOrEqual(a,b) -> ">=" + "\n" + (prettyPrint (A(a)) (i+1)) + "\n" + (prettyPrint (A(b)) (i+1))
        | BoolLess(a,b) -> "<" + "\n" + (prettyPrint (A(a)) (i+1)) + "\n" + (prettyPrint (A(b)) (i+1))
        | BoolLessOrEqual(a,b) -> "<=" + "\n" + (prettyPrint (A(a)) (i+1)) + "\n" + (prettyPrint (A(b)) (i+1))
        | Par(a) -> (prettyPrint (B(a)) i)

    let printGCommand gcommand = 
        match gcommand with
        | Condition(b,c) -> "->" + "\n" + (prettyPrint (B(b)) (i+1)) + "\n" + (prettyPrint (C(c)) (i+1))
        | Else(a,b) -> "[]" + "\n" + (prettyPrint (GC(a)) (i+1)) + "\n" + (prettyPrint (GC(b)) (i+1))
        
    match ast with
        | S(s) -> (spacer i) + s
        | A(a) -> (spacer i) + printExpr a
        | B(b) -> (spacer i) + printBool b
        | C(c) -> (spacer i) + printCommand c
        | GC(gc) -> (spacer i) + printGCommand gc
    
//let rec prettyPrint ast =
//   match ast with
//   | DeclareVar(s, a) -> s + ":=" + a
//   | Skip -> "skip"
//   | Sequence(c1,c2) -> prettyPrint(c1) + "\n" + prettyPrint(c2)

let analysis (src: string) : string =
    match parse Parser.startGCL src with
        | Ok ast ->
            Console.Error.WriteLine("> {0}", ast)
            prettyPrint (C(ast)) 0
        | Error e -> "Parse error: {0}" 