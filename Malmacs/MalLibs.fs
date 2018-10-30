module Malmacs.MalLibs

open System
open System.Collections.Generic
open System.Text
open System.Drawing
open System.Text.RegularExpressions
open System.Diagnostics
open System.Threading
open System.Threading.Tasks

open FsMiniMAL
open FsMiniMAL.Types
open FsMiniMAL.Value
open FsMiniMAL.Lexing
open FsMiniMAL.Syntax
open FsMiniMAL.Typechk


let taskCoroutineStarter name (f : memory_manager -> Value array -> Value) (mm : memory_manager) (argv : Value array) =
    let task = Task.Run(fun () -> f mm argv)

    { new IMalCoroutine with
        member x.Run(slice) = if not (task.IsCompleted) then Thread.Sleep(1)
        member x.IsFinished = task.IsCompleted
        member x.Result =
            try task.Result
            with exn -> mal_failwith mm (name + ": " + exn.InnerException.Message)
        member x.Dispose() = () }

let regexMatchCoroutineStarter (mm : memory_manager) (argv : Value array) =
    let input = to_string argv.[0]
    let pattern = to_string argv.[1]
    let options = enum<RegexOptions>(to_int argv.[2])
    let f() = Regex.Match(input, pattern, options, TimeSpan(0, 0, 10))
    let task = Task.Run(f)

    { new IMalCoroutine with
        member x.Run(slice) =
            if not task.IsCompleted then Thread.Sleep(1)
        member x.IsFinished = task.IsCompleted
        member x.Result =
            try Vobj task.Result
            with exn -> mal_failwith mm ("regexMatch: " + exn.InnerException.Message)
        member x.Dispose() = () }

let add (mal : Interpreter) =

    let regex_timeout = TimeSpan(0, 0, 1)

    mal.Do("fun printf fmt = kprintf printString fmt;")
    mal.Do("""fun printfn fmt = kprintf (fn s -> printString (s ^ "\r\n")) fmt;""")
                
    mal.Fun("regexIsMatch", ["input"; "pattern"; "options"], (fun mm (input : string) (pattern : string) (options : int) ->
        try Regex.IsMatch(input, pattern, enum<RegexOptions>(options), regex_timeout)
        with exn -> mal_failwith mm ("regex_is_match: " + exn.Message)))
    
    mal.Set("regexMatch", Vcoroutine (3, regexMatchCoroutineStarter), mal.Typeof<string -> string -> int -> Match>(["input"; "pattern"; "options"]))

    mal.Fun("regexMatches", ["input"; "pattern"; "options"], (fun mm (input : string) (pattern : string) (options : int) ->
        try
            let matches = Regex.Matches(input, pattern, enum<RegexOptions>(options), regex_timeout)
            let ary = Array.zeroCreate<Match> matches.Count
            matches.CopyTo(ary, 0)
            ary
        with exn -> mal_failwith mm ("regex_matches: " + exn.Message)))
        
    mal.Fun("matchGetGroups", (fun mm (m : Match) ->
        let groups = m.Groups
        let ary = Array.zeroCreate<Group> groups.Count
        groups.CopyTo(ary, 0)
        ary))

    mal.Fun("matchGetCaptures", (fun mm (m : Match) ->
        let captures = m.Captures
        let ary = Array.zeroCreate<Capture> captures.Count
        captures.CopyTo(ary, 0)
        ary))

    mal.Fun("matchNextMatch", (fun mm (m : Match) ->
        try m.NextMatch()
        with exn -> mal_failwith mm ("regex_match_next_match: " + exn.Message)))
        
    mal.Fun("matchGetSuccess", (fun mm (m : Match) -> m.Success))
    mal.Fun("matchGetValue", (fun mm (m : Match) -> m.Value))
    mal.Fun("matchGetIndex", (fun mm (m : Match) -> m.Index))
    mal.Fun("matchGetLength", (fun mm (m : Match) -> m.Length))
    mal.Fun("matchGetName", (fun mm (m : Match) -> m.Name))
        
    mal.Fun("groupGetCaptures", (fun mm (g : Group) ->
        let captures = g.Captures
        let ary = Array.zeroCreate<Capture> captures.Count
        captures.CopyTo(ary, 0)
        ary))

    mal.Fun("groupGetSuccess", (fun mm (g : Group) -> g.Success))
    mal.Fun("groupGetIndex", (fun mm (g : Group) -> g.Index))
    mal.Fun("groupGetLength", (fun mm (g : Group) -> g.Length))
    mal.Fun("groupGetValue", (fun mm (g : Group) -> g.Value))        
    mal.Fun("groupGetName", (fun mm (g : Group) -> g.Name))        
        
    mal.Fun("captureGetIndex", (fun mm (c : Capture) -> c.Index))
    mal.Fun("captureGetLength", (fun mm (c : Capture) -> c.Length))
    mal.Fun("captureGetValue", (fun mm (c : Capture) -> c.Value))
    
    mal.Fun("jsonParse", (fun mm s ->
        try MalJson.parse s with
        | MalJson.InvalidChar pos -> mal_failwith mm (sprintf "Invalid char at pos %d" pos)
        | MalJson.UnexpectedEof -> mal_failwith mm "Unexpected EOF"))

    mal.Fun("jsonPrint", (fun mm (singleLine : bool) (json : MalJson.json) ->
        try MalJson.print singleLine json with
        | MalJson.InvalidNumberLiteral s -> mal_failwith mm ("Invalid number literal: " + s)))
    
    //mal.Fun("boom", (fun mm () -> failwith "boom" : unit))