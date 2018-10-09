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

// --------------------------------------------------------------------------------------
// JSON parser
// --------------------------------------------------------------------------------------

type private JsonParser(jsonText:string) =

    let mutable i = 0
    let s = jsonText
    
    let buf = StringBuilder()

    // Helper functions
    let skipWhitespace() =
      while i < s.Length && Char.IsWhiteSpace s.[i] do
        i <- i + 1
    let isNumChar c =
      Char.IsDigit c || c = '.' || c='e' || c='E' || c='+' || c='-'
    let throw() =
      let msg =
        sprintf
          "Invalid JSON starting at character %d, snippet = \n----\n%s\n-----\njson = \n------\n%s\n-------" 
          i (jsonText.[(max 0 (i-10))..(min (jsonText.Length-1) (i+10))]) (if jsonText.Length > 1000 then jsonText.Substring(0, 1000) else jsonText)
      failwith msg
    let ensure cond =
      if not cond then throw()

    // Recursive descent parser for JSON that uses global mutable index
    let rec parseValue() =
        skipWhitespace()
        ensure(i < s.Length)
        match s.[i] with
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
        ensure(i < s.Length)
        match s.[i] with
        | '{' -> parseObject()
        | '[' -> parseArray()
        | _ -> throw()

    and parseString() =
        ensure(i < s.Length && s.[i] = '"')
        i <- i + 1
        while i < s.Length && s.[i] <> '"' do
            if s.[i] = '\\' then
                ensure(i+1 < s.Length)
                match s.[i+1] with
                | 'b' -> buf.Append('\b') |> ignore
                | 'f' -> buf.Append('\f') |> ignore
                | 'n' -> buf.Append('\n') |> ignore
                | 't' -> buf.Append('\t') |> ignore
                | 'r' -> buf.Append('\r') |> ignore
                | '\\' -> buf.Append('\\') |> ignore
                | '/' -> buf.Append('/') |> ignore
                | '"' -> buf.Append('"') |> ignore
                | 'u' ->
                    ensure(i+5 < s.Length)
                    let hexdigit d =
                        if d >= '0' && d <= '9' then int32 d - int32 '0'
                        elif d >= 'a' && d <= 'f' then int32 d - int32 'a' + 10
                        elif d >= 'A' && d <= 'F' then int32 d - int32 'A' + 10
                        else failwith "hexdigit"
                    let unicodeChar (s:string) =
                        if s.Length <> 4 then failwith "unicodeChar";
                        char (hexdigit s.[0] * 4096 + hexdigit s.[1] * 256 + hexdigit s.[2] * 16 + hexdigit s.[3])
                    let ch = unicodeChar (s.Substring(i+2, 4))
                    buf.Append(ch) |> ignore
                    i <- i + 4  // the \ and u will also be skipped past further below
                | _ -> throw()
                i <- i + 2  // skip past \ and next char
            else
                buf.Append(s.[i]) |> ignore
                i <- i + 1
        ensure(i < s.Length && s.[i] = '"')
        i <- i + 1
        let str = buf.ToString()
        buf.Clear() |> ignore
        str

    and parseNum() =
        let start = i
        while i < s.Length && (isNumChar s.[i]) do
            i <- i + 1
        let len = i - start
        let sub = s.Substring(start, len)
        json.Jnumber sub

    and parsePair() =
        let key = parseString()
        skipWhitespace()
        ensure(i < s.Length && s.[i] = ':')
        i <- i + 1
        skipWhitespace()
        key, parseValue()

    and parseObject() =
        ensure(i < s.Length && s.[i] = '{')
        i <- i + 1
        skipWhitespace()
        let pairs = ResizeArray<_>()
        if i < s.Length && s.[i] = '"' then
            pairs.Add(parsePair())
            skipWhitespace()
            while i < s.Length && s.[i] = ',' do
                i <- i + 1
                skipWhitespace()
                pairs.Add(parsePair())
                skipWhitespace()
        ensure(i < s.Length && s.[i] = '}')
        i <- i + 1
        json.Jobject(pairs.ToArray())

    and parseArray() =
        ensure(i < s.Length && s.[i] = '[')
        i <- i + 1
        skipWhitespace()
        let vals = ResizeArray<_>()
        if i < s.Length && s.[i] <> ']' then
            vals.Add(parseValue())
            skipWhitespace()
            while i < s.Length && s.[i] = ',' do
                i <- i + 1
                skipWhitespace()
                vals.Add(parseValue())
                skipWhitespace()
        ensure(i < s.Length && s.[i] = ']')
        i <- i + 1
        json.Jarray(vals.ToArray())

    and parseLiteral(expected, r) =
        ensure(i+expected.Length < s.Length)
        for j in 0 .. expected.Length - 1 do
            ensure(s.[i+j] = expected.[j])
        i <- i + expected.Length
        r

    // Start by parsing the top-level value
    member x.Parse() =
        let value = parseRootValue()
        skipWhitespace()
        if i <> s.Length then
            throw()
        value

    member x.ParseMultiple() =
        seq {
            while i <> s.Length do
                yield parseRootValue()
                skipWhitespace()
        }

type json with

  /// Parses the specified JSON string
  static member Parse(text) =
    JsonParser(text).Parse()

  /// Attempts to parse the specified JSON string
  static member TryParse(text) =
    try
      Some <| JsonParser(text).Parse()
    with
      | _ -> None

  /// Loads JSON from the specified stream
  static member Load(stream:Stream) =
    use reader = new StreamReader(stream)
    let text = reader.ReadToEnd()
    JsonParser(text).Parse()

  /// Loads JSON from the specified reader
  static member Load(reader:TextReader) =
    let text = reader.ReadToEnd()
    JsonParser(text).Parse()
  
  /// Parses the specified string into multiple JSON values
  static member ParseMultiple(text) =
    JsonParser(text).ParseMultiple()
