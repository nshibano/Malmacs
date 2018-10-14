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

  member x.WriteTo (w : TextWriter, singleLine : bool) =

    let newLine =
      if not singleLine then
        fun indentation plus ->
          w.WriteLine()
          System.String(' ', indentation + plus) |> w.Write
      else
        fun _ _ -> ()

    let propSep =
      if not singleLine then "\": "
      else "\":"

    let rec serialize indentation = function
      | Jnull -> w.Write "null"
      | Jtrue -> w.Write "true"
      | Jfalse -> w.Write "false"
      | Jnumber number -> w.Write number
      | Jstring s ->
          w.Write "\""
          json.JsonStringEncodeTo w s
          w.Write "\""
      | Jobject properties ->
          w.Write "{"
          for i = 0 to properties.Length - 1 do
            let k,v = properties.[i]
            if i > 0 then w.Write ","
            newLine indentation 2
            w.Write "\""
            json.JsonStringEncodeTo w k
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

  // Encode characters that are not valid in JS string. The implementation is based
  // on https://github.com/mono/mono/blob/master/mcs/class/System.Web/System.Web/HttpUtility.cs
  static member internal JsonStringEncodeTo (w:TextWriter) (value:string) =
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
          | '"'  -> w.Write "\\\""
          | '\\' -> w.Write "\\\\"
          | _    -> w.Write c

  member x.ToString (oneLine : bool) =
    let w = new StringWriter(CultureInfo.InvariantCulture)
    x.WriteTo(w, oneLine)
    w.GetStringBuilder().ToString()

  override x.ToString() = x.ToString(false)

type StringBuilder with
    member sb.Add(c : char) = sb.Append(c) |> ignore

exception InvalidCharAt of int
exception UnexpectedEof

type private Parser(s : string) =

    let mutable pos = 0

    let ensureChar() = if not (pos < s.Length) then raise UnexpectedEof

    let skip (keyword : string) =
        for i = 0 to keyword.Length - 1 do
            if not (pos + i < s.Length) then raise UnexpectedEof
            if not (s.[pos + i] = keyword.[i]) then raise (InvalidCharAt (pos + i))
        pos <- pos + keyword.Length

    let skipWhitespace() =
        while (pos < s.Length && 
               match s.[pos] with
               | '\x09' | '\x0a' | '\x0d' | '\x20' -> true
               | _ -> false) do pos <- pos + 1

    let rec parseValue() =
        skipWhitespace()
        ensureChar()
        let c = s.[pos]
        match c with
        | '"' -> json.Jstring(parseString())
        | '-' -> parseNum()
        | '{' -> parseObject()
        | '[' -> parseArray()
        | 't' -> skip "true"; Jtrue
        | 'f' -> skip "false"; Jfalse
        | 'n' -> skip "null"; Jnull
        | _ ->
            if '0' <= c && c <= '9' then
                parseNum()
            else raise (InvalidCharAt pos)

    and parseRootValue() =
        skipWhitespace()
        ensureChar()
        match s.[pos] with
        | '{' -> parseObject()
        | '[' -> parseArray()
        | _ -> raise (InvalidCharAt pos)

    and parseString() =
        skip "\""
        let buf = StringBuilder()
        let mutable cont = true
        while cont do
            ensureChar()
            let c0 = s.[pos]
            match c0 with
            | '"' ->
                cont <- false
                pos <- pos + 1
            | '\\' ->
                if pos + 1 < s.Length then
                    let c1 = s.[pos + 1]
                    match c1 with
                    | 'b' -> buf.Add('\b'); pos <- pos + 2
                    | 'f' -> buf.Add('\f'); pos <- pos + 2
                    | 'n' -> buf.Add('\n'); pos <- pos + 2
                    | 't' -> buf.Add('\t'); pos <- pos + 2
                    | 'r' -> buf.Add('\r'); pos <- pos + 2
                    | '\\' -> buf.Add('\\'); pos <- pos + 2
                    | '/' -> buf.Add('/'); pos <- pos + 2
                    | '"' -> buf.Add('"'); pos <- pos + 2
                    | 'u' ->
                        for i = 2 to 5 do
                            if not (pos + i < s.Length) then raise UnexpectedEof
                            let ci = s.[pos + i]
                            if not ('0' <= ci && ci <= '9' ||
                                    'a' <= ci && ci <= 'f' ||
                                    'A' <= ci && ci <= 'F') then raise (InvalidCharAt (pos + i))
                        buf.Add(char (Convert.ToInt32(s.Substring(pos + 2, 4), 16)))
                        pos <- pos + 6
                    | _ -> raise (InvalidCharAt (pos + 1))
                else raise UnexpectedEof
            | _ ->
                buf.Add(s.[pos])
                pos <- pos + 1
        buf.ToString()

    and parseNum() =
        let start = pos
        while (pos < s.Length &&
               (let c = s.[pos]
                ('0' <= c && c <= '9') ||
                (match c with
                 | '.' | 'e' | 'E' | '+' | '-' -> true
                 | _ -> false))) do pos <- pos + 1
        let len = pos - start
        let sub = s.Substring(start, len)
        json.Jnumber sub

    and parsePair() =
        let key = parseString()
        skipWhitespace()
        skip ":"
        skipWhitespace()
        key, parseValue()

    and parseObject() =
        skip "{"
        skipWhitespace()
        let pairs = ResizeArray()
        ensureChar()
        match s.[pos] with
        | '"' ->
            pairs.Add(parsePair())
            let mutable cont = true
            while cont do
                skipWhitespace()
                ensureChar()
                match s.[pos] with
                | ',' ->
                    pos <- pos + 1
                    skipWhitespace()
                    pairs.Add(parsePair())
                | '}' ->
                    cont <- false
                    pos <- pos + 1
                | _ -> raise (InvalidCharAt pos)
        | '}' ->
            pos <- pos + 1
        | _ -> raise (InvalidCharAt pos)
        json.Jobject(pairs.ToArray())

    and parseArray() =
        skip "["
        skipWhitespace()
        let vals = ResizeArray<_>()
        ensureChar()
        if s.[pos] = ']' then
            pos <- pos + 1
        else
            vals.Add(parseValue())
            let mutable cont = true
            while cont do
                ensureChar()
                match s.[pos] with
                | ',' ->
                    pos <- pos + 1
                    skipWhitespace()
                    vals.Add(parseValue())
                | ']' ->
                    cont <- false
                    pos <- pos + 1
                | _ -> raise (InvalidCharAt pos)
        json.Jarray(vals.ToArray())


    // Start by parsing the top-level value
    member x.Parse() =
        let value = parseRootValue()
        skipWhitespace()
        if pos <> s.Length then
            raise (InvalidCharAt pos)
        value

    member x.ParseMultiple() =
        seq {
            while pos <> s.Length do
                yield parseRootValue()
                skipWhitespace()
        }

type json with

  /// Parses the specified JSON string
  static member Parse(text) =
    Parser(text).Parse()

  /// Attempts to parse the specified JSON string
  static member TryParse(text) =
    try
      Some <| Parser(text).Parse()
    with
      | _ -> None
  
  /// Parses the specified string into multiple JSON values
  static member ParseMultiple(text) =
    Parser(text).ParseMultiple()
