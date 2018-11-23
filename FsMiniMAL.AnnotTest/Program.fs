module FsMiniMAL.Program

open System
open System.Collections.Generic
open System.Text
open System.Diagnostics
open FSharp.Reflection

open FsMiniMAL.Lexing
open Syntax
open Types
open Typechk
open Value

let src = """
type foo = Foo | Bar of int;
val l = [Foo, Bar 0];
val x = 0 + 1;
val y = x + x;
fun f x = if x < 0 then 1 else x * 100;
"""

let escape (s : string) = "\"" + s + "\""

let getUnionCaseName<'T > (x : 'T) : string =
    if FSharpType.IsUnion(typeof<'T>) then
        let a, b = FSharpValue.GetUnionFields(x, typeof<'T>)
        a.Name
    else
        failwith "not union type"

[<EntryPoint>]
let main argv =
    let lexbuf = LexBuffer.FromString src
    lexbuf.EndPos <- { lexbuf.EndPos with FileName = dummy_file_name }
    lexbuf.LocalStore.["src"] <- src
    let cmds, _ = Parser.Program Lexer.main lexbuf

    let mutable tyenv = FsMiniMAL.Top.tyenv_std
    let rec cmdLoop (cmd : Syntax.command) =
        match cmd.sc_desc with
        | SCval l
        | SCCval (l, _) -> for (_, e) in l do exprLoop e
        | SCfun l
        | SCvar l
        | SCCfun (l, _)
        | SCCvar (l, _) -> for (_, e) in l do exprLoop e
        | _ -> ()
    and exprLoop (e : Syntax.expression) =
        Syntax.expressionDo exprLoop e
        let st = e.se_loc.st.AbsoluteOffset
        let ed = e.se_loc.ed.AbsoluteOffset
        printfn "%-12s %-40s [%3d, %3d) %s" (getUnionCaseName e.se_desc) (escape (src.Substring(st, ed - st))) st ed (match e.se_type with None -> "?" | Some ty -> (fst (Printer.print_type tyenv 1000 ty)))

    List.iter cmdLoop cmds

    printfn "----"

    let tyenvs = Typechk.type_command_list ignore tyenv cmds
    tyenv <- tyenvs.[tyenvs.Length - 1]
    List.iter cmdLoop cmds

    Console.ReadKey() |> ignore
    0