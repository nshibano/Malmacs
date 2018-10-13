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

type private Parser(s : string) =

    let mutable pos = 0

    let skipWhitespace() =
        while pos < s.Length && Char.IsWhiteSpace s.[pos] do
            pos <- pos + 1

    let throw() =
      let msg =
        sprintf
          "Invalid JSON starting at character %d, snippet = \n----\n%s\n-----\njson = \n------\n%s\n-------" 
          pos (s.[(max 0 (pos-10))..(min (s.Length-1) (pos+10))]) (if s.Length > 1000 then s.Substring(0, 1000) else s)
      failwith msg
    let ensure cond =
      if not cond then throw()

    let rec parseValue() =
        skipWhitespace()
        ensure(pos < s.Length)
        match s.[pos] with
        | '"' -> json.Jstring(parseString())
        | '-' -> parseNum()
        | '{' -> parseObject()
        | '[' -> parseArray()
        | 't' -> parseLiteral("true", Jtrue)
        | 'f' -> parseLiteral("false", Jfalse)
        | 'n' -> parseLiteral("null", Jnull)
        | c ->
            if Char.IsDigit(c) then
                parseNum()
            else throw()

    and parseRootValue() =
        skipWhitespace()
        ensure(pos < s.Length)
        match s.[pos] with
        | '{' -> parseObject()
        | '[' -> parseArray()
        | _ -> throw()

    and parseString() =
        ensure(pos < s.Length && s.[pos] = '"')
        let buf = StringBuilder()
        pos <- pos + 1
        while pos < s.Length && s.[pos] <> '"' do
            if s.[pos] = '\\' then
                ensure(pos+1 < s.Length)
                match s.[pos+1] with
                | 'b' -> buf.Append('\b') |> ignore
                | 'f' -> buf.Append('\f') |> ignore
                | 'n' -> buf.Append('\n') |> ignore
                | 't' -> buf.Append('\t') |> ignore
                | 'r' -> buf.Append('\r') |> ignore
                | '\\' -> buf.Append('\\') |> ignore
                | '/' -> buf.Append('/') |> ignore
                | '"' -> buf.Append('"') |> ignore
                | 'u' ->
                    ensure(pos+5 < s.Length)
                    let hexdigit d =
                        if d >= '0' && d <= '9' then int32 d - int32 '0'
                        elif d >= 'a' && d <= 'f' then int32 d - int32 'a' + 10
                        elif d >= 'A' && d <= 'F' then int32 d - int32 'A' + 10
                        else failwith "hexdigit"
                    let unicodeChar (s:string) =
                        if s.Length <> 4 then failwith "unicodeChar";
                        char (hexdigit s.[0] * 4096 + hexdigit s.[1] * 256 + hexdigit s.[2] * 16 + hexdigit s.[3])
                    let ch = unicodeChar (s.Substring(pos+2, 4))
                    buf.Append(ch) |> ignore
                    pos <- pos + 4  // the \ and u will also be skipped past further below
                | _ -> throw()
                pos <- pos + 2  // skip past \ and next char
            else
                buf.Append(s.[pos]) |> ignore
                pos <- pos + 1
        ensure(pos < s.Length && s.[pos] = '"')
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
        ensure(pos < s.Length && s.[pos] = ':')
        pos <- pos + 1
        skipWhitespace()
        key, parseValue()

    and parseObject() =
        ensure(pos < s.Length && s.[pos] = '{')
        pos <- pos + 1
        skipWhitespace()
        let pairs = ResizeArray<_>()
        if pos < s.Length && s.[pos] = '"' then
            pairs.Add(parsePair())
            skipWhitespace()
            while pos < s.Length && s.[pos] = ',' do
                pos <- pos + 1
                skipWhitespace()
                pairs.Add(parsePair())
                skipWhitespace()
        ensure(pos < s.Length && s.[pos] = '}')
        pos <- pos + 1
        json.Jobject(pairs.ToArray())

    and parseArray() =
        ensure(pos < s.Length && s.[pos] = '[')
        pos <- pos + 1
        skipWhitespace()
        let vals = ResizeArray<_>()
        if pos < s.Length && s.[pos] <> ']' then
            vals.Add(parseValue())
            skipWhitespace()
            while pos < s.Length && s.[pos] = ',' do
                pos <- pos + 1
                skipWhitespace()
                vals.Add(parseValue())
                skipWhitespace()
        ensure(pos < s.Length && s.[pos] = ']')
        pos <- pos + 1
        json.Jarray(vals.ToArray())

    and parseLiteral(expected, r) =
        ensure(pos+expected.Length < s.Length)
        for j in 0 .. expected.Length - 1 do
            ensure(s.[pos+j] = expected.[j])
        pos <- pos + expected.Length
        r

    // Start by parsing the top-level value
    member x.Parse() =
        let value = parseRootValue()
        skipWhitespace()
        if pos <> s.Length then
            throw()
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
