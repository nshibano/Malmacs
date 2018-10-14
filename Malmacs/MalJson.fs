module Malmacs.MalJson
/// Derived from https://github.com/fsharp/FSharp.Data/blob/master/src/Json/JsonValue.fs

// --------------------------------------------------------------------------------------
// Copyright (c) Microsoft Corporation 2005-2012.
// This sample code is provided "as is" without warranty of any kind.
// We disclaim all warranties, either express or implied, including the
// warranties of merchantability and fitness for a particular purpose.
//
// A simple F# portable parser for JSON data
// --------------------------------------------------------------------------------------

open System
open System.IO
open System.Globalization
open System.Text

type json =
    | Jstring of string
    | Jnumber of string
    | Jobject of (string * json) array
    | Jarray of json array
    | Jtrue
    | Jfalse
    | Jnull
  
type private Printer() =

    member this.WriteTo (w : TextWriter, singleLine : bool, x : json) =

        let newLine =
            if not singleLine then
                fun indentation plus ->
                    w.WriteLine()
                    System.String(' ', indentation + plus) |> w.Write
            else
                fun _ _ -> ()

        let propSep =
            if not singleLine
            then "\": "
            else "\":"

        let rec serialize indentation =
            function
            | Jnull -> w.Write "null"
            | Jtrue -> w.Write "true"
            | Jfalse -> w.Write "false"
            | Jnumber number -> w.Write number
            | Jstring s ->
                w.Write "\""
                this.JsonStringEncodeTo w s
                w.Write "\""
            | Jobject properties ->
                w.Write "{"
                for i = 0 to properties.Length - 1 do
                    let k,v = properties.[i]
                    if i > 0 then w.Write ","
                    newLine indentation 2
                    w.Write "\""
                    this.JsonStringEncodeTo w k
                    w.Write propSep
                    serialize (indentation + 2) v
                newLine indentation 0
                w.Write "}"
            | Jarray elements ->
                w.Write "["
                for i = 0 to elements.Length - 1 do
                    if i > 0 then w.Write ","
                    newLine indentation 2
                    serialize (indentation + 2) elements.[i]
                if elements.Length > 0 then
                    newLine indentation 0
                w.Write "]"
    
        serialize 0 x

    member this.ToString (oneLine : bool, x : json) =
        let w = new StringWriter(CultureInfo.InvariantCulture)
        this.WriteTo(w, oneLine, x)
        w.GetStringBuilder().ToString()

    // Encode characters that are not valid in JS string. The implementation is based
    // on https://github.com/mono/mono/blob/master/mcs/class/System.Web/System.Web/HttpUtility.cs
    member this.JsonStringEncodeTo (w:TextWriter) (value:string) =
        if String.IsNullOrEmpty value then ()
        else 
            for i = 0 to value.Length - 1 do
                let c = value.[i]
                let ci = int c
                if ci >= 0 && ci <= 7 || ci = 11 || ci >= 14 && ci <= 31 then
                    w.Write("\\u{0:x4}", ci) |> ignore
                else 
                    match c with
                    | '\b' -> w.Write "\\b"
                    | '\t' -> w.Write "\\t"
                    | '\n' -> w.Write "\\n"
                    | '\f' -> w.Write "\\f"
                    | '\r' -> w.Write "\\r"
                    | '"' -> w.Write "\\\""
                    | '\\' -> w.Write "\\\\"
                    | _ -> w.Write c

let print (singleLine : bool) (json : json) =
    let w = new StringWriter()
    Printer().WriteTo(w, singleLine, json)
    w.ToString()

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

    let rec parseValue() =
        skipWhitespace()
        match s.[pos] with
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
            values.Add(parseValue())
            let mutable cont = true
            while cont do
                skipWhitespace()
                match s.[pos] with
                | ',' ->
                    pos <- pos + 1
                    skipWhitespace()
                    values.Add(parseValue())
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
        key, parseValue()
    
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

    let parseRootValue() =
        match s.[pos] with
        | '{' -> parseObject()
        | '[' -> parseArray()
        | _ -> invalidChar()

    member x.Parse() =
        try
            skipWhitespace()
            let value = parseRootValue()
            skipWhitespace()
            if pos <> s.Length then invalidChar()
            value
        with :? IndexOutOfRangeException -> raise UnexpectedEof

let parse (s : string) = Parser(s).Parse()
