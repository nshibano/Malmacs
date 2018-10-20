module FsMiniMAL.Program

open System
open System.Collections.Generic
open System.Text
open System.Diagnostics

open FsMiniMAL.Lexing
open Syntax
open Types
open Typechk
open Value

let src = """
val x = 0 + 1;
val y = x + x;
"""

[<EntryPoint>]
let main argv =
    let lexbuf = LexBuffer<char>.FromString src
    lexbuf.EndPos <- { lexbuf.EndPos with pos_fname = dummy_file_name }
    lexbuf.BufferLocalStore.["src"] <- src
    let cmds, store = Parser.Program Lexer.main lexbuf

    let tyenv = FsMiniMAL.Top.tyenv_std
    let rec cmdLoop (cmd : Syntax.command) =
        match cmd.sc_desc with
        | SCval l
        | SCCval (l, _) -> for (_, e) in l do exprLoop e
        | _ -> ()
    and exprLoop (e : Syntax.expression) =
        match e.se_desc with
        | SEid s -> printfn "%s %d-%d %s" s e.se_loc.st.AbsoluteOffset e.se_loc.ed.AbsoluteOffset (match e.se_type with None -> "n/a" | Some ty -> (fst (Printer.print_type tyenv 1000 ty)))
        | SEbegin l -> List.iter cmdLoop l
        | _ -> Syntax.expressionDo exprLoop e



    List.iter cmdLoop cmds

    printfn "----"
    Typechk.type_command_list ignore tyenv cmds |> ignore


    
    List.iter cmdLoop cmds

    Console.ReadKey() |> ignore
    0