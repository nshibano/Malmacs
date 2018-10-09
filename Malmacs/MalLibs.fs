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
open FsMiniMAL.Message

let malEnsureEditorIsOk mm (e : Editor) =
    if e.IsDisposed then mal_failwith mm "The editor has been disposed"

let editorGetTextCoroutineStarter (mm : memory_manager) (argv : value array) =
    let e = to_obj argv.[0] :?> Editor
    let doc = e.Doc
    let mutable state = 0
    let mutable i = 0
    let sb = StringBuilder()

    { new IMalCoroutine with
        member x.Run(slice) =
            if e.IsDisposed then state <- 3
            //if cancellable && mal.OperationCancelled then state <- 2
            let timestampAtStart = Environment.TickCount
            while state = 0 && Environment.TickCount - timestampAtStart < slice do
                if i < doc.RowTree.Count then
                    let row = doc.RowTree.[i]
                    sb.Add(row.String)
                    i <- i + 1
                else
                    state <- 1

        member x.IsFinished = state <> 0
        member x.Result =
            match state with
            | 1 -> of_string mm (sb.ToString())
            | 3 -> mal_failwith mm "editorGetText: The editor has been disposed."
            | _ -> dontcare()
        member x.Dispose() = () }

let setColorCoroutineStarter (mal : FsMiniMAL.Interpreter) (cancellable : bool) (mm : memory_manager) (argv : value array) =
    let e = to_obj argv.[0] :?> Editor
    let ary = to_malarray argv.[1]
    let mutable state = 0
    let mutable i = 0
    let mutable latestValue = mal.ValueOfObj<ColorInfo>(Doc.ColorInfo_Default)
    let mutable latestObj = Doc.ColorInfo_Default
    let colorInfoAt i =
        let v = ary.storage.[i]
        if LanguagePrimitives.PhysicalEquality v latestValue then
            latestObj
        else
            let x = mal.ObjOfValue<ColorInfo>(v)
            latestValue <- v
            latestObj <- x
            x
    let doc = e.Doc
    let mutable rowAccu = MeasuredTreeList<Row, RowTreeInfo>(Doc.rowTreeFunc, Doc.RowTreeInfo_Zero)

    { new IMalCoroutine with
        member x.Run(slice) =
            if e.IsDisposed then state <- 3
            let timestampAtStart = Environment.TickCount
            while state = 0 && Environment.TickCount - timestampAtStart < slice do
                if i < doc.RowTree.Count then
                    let row = doc.RowTree.[i]
                    let rowRange = Doc.getCharRangeFromRowIndex doc i
                    let colors =
                        Array.init row.SymbolCount (fun j ->
                            let ofs = rowRange.Begin + row.CharOffsets.[j]
                            if ofs < ary.count then
                                colorInfoAt ofs
                            else
                                Doc.ColorInfo_Default)
                    rowAccu <- rowAccu.Add({ row with Colors = colors })
                    i <- i + 1
                else
                    let newDoc = { doc with RowTree = rowAccu }
                    if LanguagePrimitives.PhysicalEquality e.Doc doc then
                        e.Doc <- newDoc
                        e.TextArea.Invalidate()
                        state <- 1
                    else
                        state <- 2

        member x.IsFinished = state <> 0
        member x.Result =
            match state with
            | 1 -> Value.unit
            | 2 -> mal_failwith mm "editorSetColor: The text has been modified."
            | 3 -> mal_failwith mm "editorSetColor: The editor has been disposed."
            | _ -> dontcare()
        member x.Dispose() = () }

let taskCoroutineStarter name (f : memory_manager -> value array -> value) (mm : memory_manager) (argv : value array) =
    let task = Task.Run(fun () -> f mm argv)

    { new IMalCoroutine with
        member x.Run(slice) = if not (task.IsCompleted) then Thread.Sleep(1)
        member x.IsFinished = task.IsCompleted
        member x.Result =
            try task.Result
            with exn -> mal_failwith mm (name + ": " + exn.InnerException.Message)
        member x.Dispose() = () }

let regexMatchCoroutineStarter (mm : memory_manager) (argv : value array) =
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

let add (repl : Repl) =
    let mal = repl.Mal

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
    
    mal.Fun("getEditor", (fun mm () ->
        match repl.SelectedEditor with
        | Some e -> e
        | None -> mal_failwith mm "There is no selected editor."))
    mal.Fun("getEditors", (fun mm () -> repl.Editors.ToArray()))

    mal.Fun("editorGetFilename", (fun mm (e : Editor) ->
        malEnsureEditorIsOk mm e
        match e.TextFileHandle with
        | Some h -> h.OriginalPath
        | None -> ""))

    mal.Set("editorGetText", Vcoroutine (1, editorGetTextCoroutineStarter), mal.Typeof<Editor -> string>())

    mal.Fun("editorSetText", (fun mm (e : Editor) (s : string) ->
        malEnsureEditorIsOk mm e
        e.EditorText <- s))
    mal.Fun("editorUpdateHighlighting", (fun mm (e : Editor) -> repl.RequestHighlighting(e)))
    mal.Set("editorSetColor", Vcoroutine (2, setColorCoroutineStarter mal false), mal.Typeof<Editor -> ColorInfo array -> unit>())
    mal.Fun("colorOfRgb", fun mm i -> Color.FromArgb(0xFF000000 ||| i))

    let parse src =
        let lexbuf = LexBuffer<char>.FromString src
        lexbuf.EndPos <- { lexbuf.EndPos with pos_fname = dummy_file_name }
        lexbuf.BufferLocalStore.["src"] <- src
        try
            let cmds, _ = Parser.Program Lexer.main lexbuf
            Ok cmds
        with
        | LexHelper.Lexical_error lex_err ->
            let loc = { src = src; st = lexbuf.StartPos; ed = lexbuf.EndPos }
            Error (LexicalError (lex_err, loc))
        | Failure "parse error" ->
            let loc = { src = src; st = lexbuf.StartPos; ed = lexbuf.EndPos }
            Error (SyntaxError loc)
    
    let rangeOfLocation (loc : location) =
        (loc.st.AbsoluteOffset, loc.ed.AbsoluteOffset)

    mal.Fun("malTypecheck", fun mm src ->
        match parse src with
        | Error (LexicalError (_, loc) as msg) ->
            let s = Printer.print_message Printer.lang.Ja 1000 msg
            let s = Regex.Replace(s, @"[ \t\r\n]+", " ")
            Some (s, rangeOfLocation loc)
        | Error (SyntaxError loc as msg) ->
            let s = Printer.print_message Printer.lang.Ja 1000 msg 
            let s = Regex.Replace(s, @"[ \t\r\n]+", " ")
            Some (s, rangeOfLocation loc)
        | Error _ -> dontcare()
        | Ok cmds ->
            let warning_sink (err : type_error_desc, loc : location) = ()
            let tyenv = Typechk.tyenv_clone mal.Tyenv

            try
                Typechk.type_command_list warning_sink tyenv cmds |> ignore
                None
            with Type_error (err, loc) ->
                let s = Printer.print_typechk_error Printer.lang.Ja 1000 err
                let s = Regex.Replace(s, @"[ \t\r\n]+", " ")
                Some (s, rangeOfLocation loc))
