// Derived from https://github.com/fsharp/FSharp.Data/blob/master/src/Json/JsonValue.fs

// --------------------------------------------------------------------------------------
// Copyright (c) Microsoft Corporation 2005-2012.
// This sample code is provided "as is" without warranty of any kind.
// We disclaim all warranties, either express or implied, including the
// warranties of merchantability and fitness for a particular purpose.
//
// A simple F# portable parser for JSON data
// --------------------------------------------------------------------------------------

module Malmacs.MalJson

open System
open System.Globalization
open System.Text
open System.Text.RegularExpressions

type json =
    | Jstring of string
    | Jnumber of string
    | Jobject of (string * json) array
    | Jarray of json array
    | Jtrue
    | Jfalse
    | Jnull

exception InvalidChar of int
exception UnexpectedEof

type private Parser(s : string) =

    let mutable pos = 0

    let invalidChar() = raise (InvalidChar pos)

    let skip (keyword : string) =
        for c in keyword do
            if s.[pos] <> c then invalidChar()
            pos <- pos + 1

    let skipWhitespace() =
        while (pos < s.Length && 
               match s.[pos] with
               | '\x09' | '\x0a' | '\x0d' | '\x20' -> true
               | _ -> false) do pos <- pos + 1

    let skipDigitStar() =
        while (pos < s.Length &&
               let c = s.[pos]
               '0' <= c && c <= '9') do pos <- pos + 1

    let skipDigitPlus() =
        let c = s.[pos]
        if not ('0' <= c && c <= '9') then invalidChar()
        pos <- pos + 1
        skipDigitStar()
    
    let parseNumber() =
        let start = pos

        // int
        if s.[pos] = '-' then
            pos <- pos + 1
        let c = s.[pos]
        if c = '0' then
            pos <- pos + 1
        elif '1' <= c && c <= '9' then
            pos <- pos + 1
            skipDigitStar()
        else invalidChar()

        // frac
        if pos < s.Length then
            let c = s.[pos]
            if c = '.' then
                pos <- pos + 1
                skipDigitPlus()

        // exp
        if pos < s.Length then
            let c = s.[pos]
            if c = 'e' || c = 'E' then
                pos <- pos + 1
                let c = s.[pos]
                if c = '+' || c = '-' then
                    pos <- pos + 1
                skipDigitPlus()

        json.Jnumber (s.Substring(start, pos - start))
    
    let parseEscapedChar() =
        let c = s.[pos]
        if c = 'u' then
            pos <- pos + 1
            let start = pos
            for i = 0 to 3 do
                let c = s.[pos]
                if not ('0' <= c && c <= '9' ||
                        'a' <= c && c <= 'f' ||
                        'A' <= c && c <= 'F') then invalidChar()
                pos <- pos + 1
            char (Int32.Parse(s.Substring(start, 4), NumberStyles.HexNumber))
        else
            let c =
                match c with
                | 'b' -> '\b'
                | 'f' -> '\f'
                | 'n' -> '\n'
                | 't' -> '\t'
                | 'r' -> '\r'
                | '\\' -> '\\'
                | '/' -> '/'
                | '"' -> '"'
                | _ -> invalidChar()
            pos <- pos + 1
            c

    let parseString() =
        let buf = StringBuilder()
        skip "\""
        let mutable cont = true
        while cont do
            let c0 = s.[pos]
            match c0 with
            | '"' ->
                cont <- false
                pos <- pos + 1
            | '\\' ->
                pos <- pos + 1
                buf.Append(parseEscapedChar()) |> ignore
            | _ ->
                buf.Append(s.[pos]) |> ignore
                pos <- pos + 1
        buf.ToString()

    let rec parseValue (isRoot : bool) =
        skipWhitespace()
        let c = s.[pos]
        if isRoot && not (c = '{' || c = '[') then
            invalidChar()
        match c with
        | '"' -> Jstring (parseString())
        | '-' -> parseNumber()
        | '{' -> parseObject()
        | '[' -> parseArray()
        | 't' -> skip "true"; Jtrue
        | 'f' -> skip "false"; Jfalse
        | 'n' -> skip "null"; Jnull
        | c ->
            if '0' <= c && c <= '9' then
                parseNumber()
            else invalidChar()

    and parseArray() =
        let values = ResizeArray()
        skip "["
        skipWhitespace()
        if s.[pos] = ']' then
            pos <- pos + 1
        else
            values.Add(parseValue false)
            let mutable cont = true
            while cont do
                skipWhitespace()
                match s.[pos] with
                | ',' ->
                    pos <- pos + 1
                    skipWhitespace()
                    values.Add(parseValue false)
                | ']' ->
                    cont <- false
                    pos <- pos + 1
                | _ -> invalidChar()
        json.Jarray(values.ToArray())

    and parsePair() =
        let key = parseString()
        skipWhitespace()
        skip ":"
        skipWhitespace()
        key, parseValue false
    
    and parseObject() =
        let pairs = ResizeArray()
        skip "{"
        skipWhitespace()
        match s.[pos] with
        | '"' ->
            pairs.Add(parsePair())
            let mutable cont = true
            while cont do
                skipWhitespace()
                match s.[pos] with
                | ',' ->
                    pos <- pos + 1
                    skipWhitespace()
                    pairs.Add(parsePair())
                | '}' ->
                    cont <- false
                    pos <- pos + 1
                | _ -> invalidChar()
        | '}' ->
            pos <- pos + 1
        | _ -> invalidChar()
        json.Jobject(pairs.ToArray())

    member x.Parse() =
        try
            let value = parseValue true
            skipWhitespace()
            if pos < s.Length then invalidChar()
            value
        with :? IndexOutOfRangeException -> raise UnexpectedEof

let parse (s : string) = Parser(s).Parse()

exception InvalidNumberLiteral of string

let reNumberLiteral = Regex(@"\A-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?\z", RegexOptions.ExplicitCapture)

type private Printer(singleLine : bool) =

    let sb = StringBuilder()
    let mutable indentation = 0
    let indentationIncrease = 2

    let write (s : string) =
        sb.Append(s) |> ignore

    let newLine =
        if not singleLine then
            fun () ->
                sb.Append("\r\n") |> ignore
                sb.Append(' ', indentation) |> ignore
        else fun () -> ()

    let propSep =
        if not singleLine
        then "\": "
        else "\":"
    
    // Encode characters that are not valid in JS string. The implementation is based
    // on https://github.com/mono/mono/blob/master/mcs/class/System.Web/System.Web/HttpUtility.cs
    let writeChars(chars : string) =
        for c in chars do
            let ci = int c
            if ci >= 0 && ci <= 7 || ci = 11 || ci >= 14 && ci <= 31 then
                Printf.bprintf sb "\\u%04x" ci
            else 
                match c with
                | '\b' -> write "\\b"
                | '\t' -> write "\\t"
                | '\n' -> write "\\n"
                | '\f' -> write "\\f"
                | '\r' -> write "\\r"
                | '"' -> write "\\\""
                | '\\' -> write "\\\\"
                | _ -> sb.Append(c) |> ignore

    let rec loop (json : json) =
        match json with
        | Jnull -> write "null"
        | Jtrue -> write "true"
        | Jfalse -> write "false"
        | Jnumber number ->
            if not (reNumberLiteral.IsMatch(number)) then
                raise (InvalidNumberLiteral number)
            write number
        | Jstring s ->
            write "\""
            writeChars s
            write "\""
        | Jobject properties ->
            write "{"
            indentation <- indentation +  indentationIncrease
            for i = 0 to properties.Length - 1 do
                let k, v = properties.[i]
                if i > 0 then write ","
                newLine()
                write "\""
                writeChars k
                write propSep
                loop v
            indentation <- indentation - indentationIncrease
            newLine()
            write "}"
        | Jarray elements ->
            write "["
            indentation <- indentation + indentationIncrease
            for i = 0 to elements.Length - 1 do
                if i > 0 then write ","
                newLine()
                loop elements.[i]
            indentation <- indentation - indentationIncrease
            if elements.Length > 0 then
                newLine()
            write "]"

    member this.Print(json : json) =
        loop json
        sb.ToString()

let print (singleLine : bool) (json : json) = Printer(singleLine).Print(json)

let private invalidOperation() = raise (InvalidOperationException())

let rec find (json : json) (path : string list) =
    match json, path with
    | Jobject fields, hd :: tl -> find (snd (Array.find (fun pair -> fst pair = hd) fields)) tl
    | _, [] -> json
    | _ -> invalidOperation()

let toInt (json : json) =
    match json with
    | Jnumber s -> int s
    | _ -> invalidOperation()

let toString (json : json) =
    match json with
    | Jstring s -> s
    | _ -> invalidOperation()

let toStringArray (json : json) =
    match json with
    | Jarray ary -> Array.map toString ary
    | _ -> invalidOperation()

let toBool (json : json) =
    match json with
    | Jtrue -> true
    | Jfalse -> false
    | _ -> invalidOperation()