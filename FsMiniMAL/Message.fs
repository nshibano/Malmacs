namespace FsMiniMAL

open Syntax
open Types
open Value

type Message =
    | LexicalError of LexHelper.lexical_error_desc * location
    | SyntaxError of location
    | TypeError of Typechk.type_error_desc * location

    | EvaluationComplete of tyenv * MalValue * type_expr
    | NewValues of tyenv * (string * MalValue * value_info) list 
    | TypeDefined of string list
    | ExceptionDefined of string
    | TypeHidden of string
    | ValueRemoved of string
    
    | UncaughtException of tyenv * MalValue
    | UncatchableException of string