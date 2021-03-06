﻿module FsMiniMAL.Syntax

open System.Collections.Generic

open FsLexYaccLite.Lexing

type location = { src : string; st : Position; ed : Position }

let dummyLoc name =
    let pos =
        { Line = 0
          StartOfLine = 0
          AbsoluteOffset = 0 }
    { src = ""; st = pos; ed = pos }

type type_expr = 
    { st_desc : type_desc
      st_loc : location }

and type_desc = 
    | STvar of string
    | STarrow of type_expr * type_expr
    | STtuple of type_expr list
    | STconstr of string * type_expr list

type type_kind = 
    | SKabbrev of type_expr
    | SKvariant of (string * type_expr list) list
    | SKrecord of (string * type_expr * access) list

type typedef = 
    { sd_name : string
      sd_params : string list
      sd_kind : type_kind
      sd_loc : location }

type list_kind = LKlist | LKarray

type pattern = 
    { mutable sp_desc : pattern_desc
      sp_loc : location }

and pattern_desc =
    | SPid of string
    | SPint of string
    | SPfloat of float
    | SPchar of char
    | SPstring of string
    | SPtuple of pattern list
    | SPlist of list_kind * pattern list
    | SPas of pattern * string
    | SPany
    | SPtype of pattern * type_expr
    | SPor of pattern * pattern
    // before typecheck only
    | SPapply of string * pattern
    | SPrecord of (string * pattern) list
    // after typecheck only
    | SPblock of int * pattern list

type regexp = 
    | Alt of regexp list
    | Seq of regexp list
    | Inp of input
    | Star of regexp
    | Macro of string

and input =
    | Alphabet of int
    | Any 
    | NotCharSet of Set<int>

let Alphabet_Epsilon = -1
let Alphabet_Eof = -2
let Alphabet_Others = -3

type DfaNode = 
    { Id : int
      Transitions : Dictionary<int, DfaNode>
      Accepted : int option }

type expression = 
    { mutable se_desc : expression_desc
      se_loc : location
      mutable se_type : Types.type_expr option }

and expression_desc = 
    | SEid of string
    | SEint of string
    | SEfloat of float
    | SEchar of char
    | SEtuple of expression list
    | SElist of list_kind * expression list
    | SEstring of string
    | SEapply of expression * expression list
    | SEfn of pattern list * expression
    | SEbegin of command list
    | SEcase of expression * (pattern * expression option * expression) list
    | SEtry of expression * (pattern * expression option * expression) list // In try expression, when clause is not supported. But parser recognize it for better error reporting.
    | SEifthenelse of expression * expression * expression option
    | SEset of string * expression
    | SEfor of string * expression * dirflag * expression * expression
    | SEwhile of expression * expression
    | SEtype of expression * type_expr
    // before typecheck only
    | SErecord of orig : expression option * fields : (string * expression) list
    | SEgetfield of expression * string
    | SEsetfield of expression * string * expression
    // after typecheck only
    | SEurecord of expression option * (int * access * expression) list
    | SEconstr of int * expression list
    | SEformat of PrintfFormat.PrintfCommand list

and command = 
    { mutable sc_desc : command_desc
      sc_loc : location }

and command_desc = 
    | SCexpr of expression
    | SCval of (pattern * expression) list
    | SCfun of (string * expression) list // The expression for this case is always SEfn
    | SCvar of (string * expression) list
    | SCtype of typedef list
    | SChide of string
    | SCremove of string
    | SCexn of string * type_expr list
    | SClex of lex_def list
    // after typecheck only
    | SCCexpr of expression * Types.type_expr
    | SCCval of (pattern * expression) list * (string * Types.value_info) list
    | SCCfun of (string * expression) list * (string * Types.value_info) list
    | SCCvar of (string * expression) list * (string * Types.value_info) list
    | SCClex of (string * string list * HashSet<int> * DfaNode * expression array * location * Types.value_info) array

and lex_def =
    | Macro_def of string * regexp
    | Rules_def of (string * string list * (regexp * expression) list * location) list

let describe_location (loc : location) =
    let {src = input; st = st; ed = ed} = loc
    let sb = System.Text.StringBuilder()
    let pf fmt = Printf.bprintf sb fmt

    pf "Line "

    if not (st.AbsoluteOffset < input.Length) then
        pf "%d, unexpected EOF" (st.Line + 1)
    elif st.Line = ed.Line then
        // when range is in one line, display char range
        pf "%d, char %d-%d: \"%s\"" (st.Line + 1) (st.AbsoluteOffset - st.StartOfLine + 1) (ed.AbsoluteOffset - ed.StartOfLine) (input.Substring(st.AbsoluteOffset, (ed.AbsoluteOffset - st.AbsoluteOffset)))
    else             
        let is_nonwhitespace c = not (System.Char.IsWhiteSpace(c))

        let end_of_first_token =
            let mutable i = st.AbsoluteOffset
            while is_nonwhitespace input.[i] && (i + 1) < ed.AbsoluteOffset do
                i <- i + 1
            i
             
        let start_of_last_token =
            let mutable i = ed.AbsoluteOffset
            while is_nonwhitespace input.[i - 1] && st.AbsoluteOffset <= i do
                i <- i - 1
            i

        let first_token = input.Substring(st.AbsoluteOffset, end_of_first_token - st.AbsoluteOffset)
        let last_token = input.Substring(start_of_last_token, ed.AbsoluteOffset - start_of_last_token)
        
        pf "%d, char %d to line %d, char %d: \"%s ... %s\"" (st.Line + 1) (st.AbsoluteOffset - st.StartOfLine + 1) (ed.Line + 1) (ed.AbsoluteOffset - ed.StartOfLine + 1) first_token last_token
        
    sb.ToString()

let expressionDo (f : expression -> unit) (e : expression) =
    match e.se_desc with
    | SEid _
    | SEint _
    | SEfloat _
    | SEchar _ -> ()
    | SEtuple l
    | SElist (_, l) -> List.iter f l
    | SEstring _ -> ()
    | SEapply (e_func, e_args) -> f e_func; List.iter f e_args
    | SEfn (_, e) -> f e
    | SEbegin _ -> ()
    | SEcase (e, cases)
    | SEtry (e, cases) -> f e; List.iter (fun (_, e_when, e) -> Option.iter f e_when; f e) cases
    | SEifthenelse (e1, e2, e3) -> f e1; f e2; Option.iter f e3
    | SEset (_, e) -> f e
    | SEfor (_, e1, _, e2, e3) -> f e1; f e2; f e3
    | SEwhile (e1, e2) -> f e1; f e2
    | SEtype (e, _) -> f e
    | SErecord (e, l) -> Option.iter f e; List.iter (fun (_, e) -> f e) l
    | SEgetfield (e, _) -> f e
    | SEsetfield (e1, _, e2) -> f e1; f e2
    | SEurecord (orig, fields) -> Option.iter f orig; List.iter (fun (_, _, e) -> f e) fields
    | SEconstr (_, l) -> List.iter f l
    | SEformat _ -> ()
