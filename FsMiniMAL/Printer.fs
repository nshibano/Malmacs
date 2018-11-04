// This is re-implementation of Format module of OCaml.
module FsMiniMAL.Printer

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Text
open Printf

open Types
open Unify
open Typechk
open Value

let print_list p sep = 
    function 
    | [] -> ()
    | hd :: tl -> 
        p hd
        List.iter (fun a -> sep(); p a) tl

let escapedChar c =
    if c = '"' then
        "\\\""
    elif c = '\\' then
        "\\\\"
    elif '\x20' <= c then
        String(c, 1)
    else
        match c with
        | '\b' -> "\\b"
        | '\t' -> "\\t"
        | '\n' -> "\\n"
        | '\r' -> "\\r"
        | _ -> sprintf "\\%03d" (int c)
    
type Node = 
    | Text of StringBuilder
    | Section of Section

and Section = 
    { Kind : SectionKind
      Indent : int
      Items : Node array
      mutable Size : int }

and SectionKind =
    | Flow
    | Vertical

let createSection kind indent (nodes : Node array) =
    Section
        { Kind = kind
          Indent = indent
          Items = nodes
          Size = 0 }

let rec weld (node : Node) (s : string) =
    match node with
    | Text sb -> sb.Add(s) |> ignore
    | Section sect ->
        weld sect.Items.[sect.Items.Length - 1] s

let rec preWeld (s : string) (node : Node) =
    match node with
    | Text sb -> sb.Insert(0, s) |> ignore
    | Section sect ->
        preWeld s sect.Items.[0]

let parenthesize (node : Node) =
    let node = createSection Flow 1 [| node |]
    preWeld "(" node
    weld node ")"
    node

type ValuePrinter(tyenv : tyenv, limit : int) =
    let stringLimit = 1000

    let mutable charCounter = 0

    let textNode (s : string) =
        charCounter <- charCounter + s.Length
        Text (StringBuilder(s))
    
    let invalidNode() = textNode "<invalid>"
    let abstrNode() = textNode "<abstr>"

    let listNode (lp : string) (rp : string) (accu : List<Node>) =

        for i = 0 to accu.Count - 2 do
            weld accu.[i] ","
        
        if accu.Count = 0 then
            textNode (lp + rp)
        else
            let node = createSection Flow lp.Length (accu.ToArray())
            preWeld lp node
            weld node rp
            node

    let rec value_loop (path : ImmutableHashSet<MalValue>) (level : int) (ty : type_expr) (value : MalValue) : Node =
        match repr ty, value.Kind with
        | Tarrow _, (MalValueKind.FUNC|MalValueKind.KFUNC|MalValueKind.PARTIAL|MalValueKind.CLOSURE|MalValueKind.COROUTINE) -> textNode "<fun>"
        | Ttuple [], MalValueKind.INT ->
            if toInt value = 0 then
                textNode "()"
            else invalidNode()
        | Tconstr(type_id.INT, []), MalValueKind.INT ->
            let s = (toInt value).ToString()
            let s =
                if 0 < level && s.[0] = '-' then
                    "(" + s + ")"
                else s
            textNode s
        | Tconstr(type_id.CHAR, []), MalValueKind.INT ->
            let i = toInt value
            if int Char.MinValue <= i && i <= int Char.MaxValue then
                let s = escapedChar (char i)
                textNode ("'" + s + "'")
            else invalidNode()
        | Tconstr(type_id.FLOAT, []), MalValueKind.FLOAT ->
            let x = toFloat value
            let s = string_of_float x
            let s =
                if 0 < level && s.[0] = '-' then
                    "(" + s + ")"
                else s
            textNode s
        | Tconstr (type_id.STRING, []), MalValueKind.STRING ->
            let s = toString value
            if s.Length < stringLimit then
                let sb = StringBuilder()
                sb.Add('"')
                for i = 0 to s.Length - 1 do
                    sb.Add(escapedChar s.[i])
                sb.Add('"')
                textNode (sb.ToString())
            else
                let sb = StringBuilder()
                for i = 0 to stringLimit - 1 do
                    sb.Add(escapedChar s.[i])
                textNode (sprintf "<string len=%d \"%s\"...>" s.Length (sb.ToString()))
        | _ when path.Contains(value) ->
                textNode "..."
        | Ttuple l, _ ->
            try
                list_loop (path.Add(value)) "(" ")" (Seq.zip l (getFields value))
            with _ -> invalidNode()
        | Tconstr(type_id.ARRAY, [a]), MalValueKind.ARRAY ->
            let ary = value :?> MalArray
            let items = seq { for i = 0 to ary.Count - 1 do yield (a, ary.Storage.[i]) }
            list_loop (path.Add(value)) "[|" "|]" items
        | Tconstr(type_id.LIST, [a]), _ ->
            let accu = List()
            let mutable path = path
            let mutable value = value
            let mutable cont = true
            while cont do
                if value.Kind = MalValueKind.BLOCK && getTag value = 1 && (getFields value).Length = 2 then
                    if charCounter < limit then
                        let fields = getFields value
                        let hd, tl = fields.[0], fields.[1]
                        path <- path.Add(value)
                        accu.Add(value_loop path 0 a hd)
                        value <- tl
                    else
                        accu.Add(textNode "...")
                        cont <- false
                else cont <- false
            listNode "[" "]" accu
        | Tconstr(type_id.EXN, _), _ ->
            try
                let tag = getTag value
                let name, info = tyenv.exn_constructors.[tag]
                let fields = getFields value
                variantNode (path.Add(value)) level name (info.ci_args) fields
            with _ -> invalidNode()
        | Tconstr(id, tyl), _ ->
            match tyenv.types_of_id.TryFind id with
            | None -> invalidNode()
            | Some info ->
                let sl = List.zip info.ti_params tyl
                match info.ti_kind with
                | Kbasic ->
                    if value.Kind = MalValueKind.OBJ then
                        if (value :?> MalObj).Obj.GetType() = typeof<System.Text.RegularExpressions.Match> then
                            let m = (value :?> MalObj).Obj :?> System.Text.RegularExpressions.Match
                            let maxPreviewLength = 10
                            if m.Success then
                                if m.Length < maxPreviewLength then
                                    textNode (sprintf "<match Success=%b Index=%d Len=%d Value=\"%s\">" m.Success m.Index m.Length m.Value)
                                else
                                    textNode (sprintf "<match Success=%b Index=%d Len=%d Value=\"%s\"...>" m.Success m.Index m.Length (m.Value.Substring(0, maxPreviewLength)))
                            else textNode "<match Success=false>"
                        else abstrNode()
                    else abstrNode()
                | Kabbrev ty -> value_loop path level (subst sl ty) value
                | Kvariant casel ->
                    try
                        let tag = Value.getTag value
                        let name, _, fieldGenTypes = List.find (fun (_, tag', _) -> tag = tag') casel
                        let fieldTypes = List.map (fun ty -> subst sl ty) fieldGenTypes
                        let fields = Value.getFields value
                        variantNode (path.Add(value)) level name fieldTypes fields
                    with _ -> invalidNode()
                | Krecord l ->
                    try
                        let fields = Value.getFields value
                        let path = path.Add(value)
                        let accu = List()
                        List.iteri (fun i (label, ty_gen, _) ->
                            let labelNode = textNode (label + " =")
                            let valueNode = value_loop path 0 (subst sl ty_gen) fields.[i]
                            accu.Add(createSection Flow 1 [|labelNode; valueNode|])) l
                        listNode "{" "}" accu
                    with _ -> invalidNode()
        | _ -> invalidNode()

    and list_loop (path : ImmutableHashSet<MalValue>) lp rp (items : (type_expr * MalValue) seq) : Node =

        let accu = List()

        let enum = items.GetEnumerator()
        while
            (match enum.MoveNext(), charCounter < limit with
                | true, true ->
                    let ty, v = enum.Current
                    accu.Add(value_loop path 0 ty v)
                    true
                | true, false ->
                    accu.Add(textNode "...")
                    false
                | false, _ -> false) do ()

        listNode lp rp accu
    
    and variantNode (path : ImmutableHashSet<MalValue>) (level : int) (name : string) (fieldTypes : type_expr list) (fields : MalValue array) =
        try
            if fieldTypes.Length <> fields.Length then failwith "invalid value"
            if fieldTypes.Length = 0 then
                textNode name
            else
                let labelNode = textNode name
                let fieldsNode =
                    (match fieldTypes with
                    | [ ty' ] -> value_loop path 1 (fieldTypes.[0]) fields.[0]
                    | _ -> list_loop path "(" ")" (Seq.zip fieldTypes fields))
                let node = createSection Flow 0 [| labelNode; fieldsNode |]
                if level > 0 then parenthesize node else node
        with _ -> invalidNode()
    
    member this.ValueLoop ty value = value_loop (ImmutableHashSet.Create<MalValue>(Misc.PhysicalEqualityComparer)) 0 ty value

let textNode (s : string) = Text (StringBuilder(s))

let node_of_value (tyenv : tyenv) ty value = ValuePrinter(tyenv, 1000).ValueLoop ty value

let node_of_type_expr (tyenv : tyenv) name_of_var is_scheme prio ty =
    
    let rec loop top prio ty =
        match repr ty with
        | Tvar tv ->
            let prefix = 
                if tv.level <> generic_level && is_scheme
                then "'_"
                else "'"
            textNode (prefix + name_of_var tv)
        | Tarrow _ ->
            let rec flatten ty =
                match repr ty with
                | Tarrow (name, ty1, ty2) -> (name, ty1) :: flatten ty2
                | _ -> [("", ty)]
            let tyl = Array.ofList (flatten ty)
            let accu = List()
            for i = 0 to tyl.Length - 2 do
                let ty_accu = List()
                let name_i, ty_i = tyl.[i]
                let termNode = loop top 1 ty_i
                if name_i <> "" && top then
                    preWeld (name_i + ":") termNode
                weld termNode " ->"
                ty_accu.Add(termNode)
                accu.Add(createSection Flow 0 (ty_accu.ToArray()))
            let _, ty_final = tyl.[tyl.Length - 1]
            accu.Add(loop top 0 ty_final)
            let node = createSection Flow 0 (accu.ToArray())
            if prio > 0 then
                parenthesize node
            else node
        | Ttuple [] -> textNode "unit"
        | Ttuple l ->
            let accu = List<Node>()
            let star() =
                weld accu.[accu.Count - 1] " *"
            print_list (fun ty -> accu.Add(loop false 2 ty)) star l
            let node = createSection Flow 0 (accu.ToArray())
            if prio > 1 then
                parenthesize node
            else node
        | Tconstr(type_id.EXN, []) -> textNode "exn"
        | Tconstr(id, []) ->
            match tyenv.types_of_id.TryFind id with
            | Some ty -> textNode ty.ti_name
            | None -> dontcare()
        | Tconstr(id, ([ ty ])) ->
            match tyenv.types_of_id.TryFind id with
            | Some ti ->
                let accu = List()
                accu.Add(loop false 2 ty)
                accu.Add(textNode ti.ti_name)
                createSection Flow 0 (accu.ToArray())
            | None -> dontcare()
        | Tconstr(id, l) ->
            match tyenv.types_of_id.TryFind id with
            | Some ti ->
                let accu = List()
                let comma() =
                    weld accu.[accu.Count - 1] ","
                print_list (fun ty -> accu.Add(loop false 0 ty)) comma l                
                createSection Flow 0 [| parenthesize (createSection Flow 0 (accu.ToArray())); textNode ti.ti_name |]
            | None -> dontcare()
    loop true prio ty

let create_tvar_assoc_table () = 
    let dict = Dictionary<type_var, string>(Misc.PhysicalEqualityComparer)
    (fun tv ->
        match dict.TryGetValue(tv) with
        | true, name -> name
        | false, _ ->
            let name = 
                let n = dict.Count / 26
                let c = dict.Count % 26
                if n > 0
                then System.Char.ConvertFromUtf32(c + 97) + string n
                else System.Char.ConvertFromUtf32(c + 97)
            dict.Add(tv, name)
            name)

let node_of_scheme (tyenv : tyenv) ty =
    let name_of_var = create_tvar_assoc_table()
    node_of_type_expr tyenv name_of_var true 0 ty

let node_of_type (tyenv : tyenv) name_of_var ty =
    node_of_type_expr tyenv name_of_var false 0 ty

let update_sizes (elem : Node) =

    let rec loop (sect : Section) =
        sect.Size <- 0

        for elem in sect.Items do
            match elem with
            | Text s -> 
                sect.Size <- sect.Size + s.Length
            | Section sub ->
                loop sub
                sect.Size <- sect.Size + sub.Size
        
        sect.Size <- sect.Size + sect.Items.Length - 1
    
    match elem with
    | Section s -> loop s
    | Text _ -> ()

let string_of_node cols node =
    let buf = List<struct (char * int16)>()

    let mutable col = 0

    let add_spaces (level : int16) (n : int) =
        for i = 0 to n - 1 do
            buf.Add(struct (' ', ~~~level))
        col <- col + n
    
    let add_string (level : int16) (s : string) =
        for c in s do
            buf.Add(struct (c, level))
        col <- col + s.Length

    let rec loop level indent vertical =
        function
        | Text sb ->
            add_string level (sb.ToString())
        | Section box ->
            let vertical = box.Kind = Vertical && cols - col < box.Size
            let indent = indent + box.Indent
            for i = 0 to box.Items.Length - 1 do
                if i <> 0 then
                    if vertical || cols - col < (1 + match box.Items.[i] with Text sb -> sb.Length | Section s -> s.Size)
                    then
                        add_string level "\r\n"
                        col <- 0
                        add_spaces (level + 1s) indent
                    else add_spaces (level + 1s) 1
                loop (level + 1s) indent vertical box.Items.[i]
    
    loop 0s 0 false node

    let buf = buf.ToArray()
    let s = String(Array.map (fun struct (c, _) -> c) buf)
    let colors = Array.map (fun struct (_, color) -> color) buf
    (s, colors)

let print_value_without_type_colored define cols ty value =
    let e = node_of_value define ty value
    update_sizes e
    string_of_node cols e

let print_value_without_type define cols ty value = fst (print_value_without_type_colored define cols ty value)

let print_value_colored define cols ty value =
    let accu1 = List()
    accu1.Add(textNode "- :")
    let ty_node = node_of_scheme define ty
    weld ty_node " ="
    accu1.Add(ty_node)
    let node1 = createSection Flow 0 (accu1.ToArray())

    let accu2 = List()
    accu2.Add(node1)
    accu2.Add(node_of_value define ty value)
    let node2 = createSection Flow 1 (accu2.ToArray())

    update_sizes node2
    string_of_node cols node2

let print_value define cols ty value = fst (print_value_colored define cols ty value)

let print_type tyenv cols ty =
    let node = node_of_type tyenv (create_tvar_assoc_table()) ty
    update_sizes node
    string_of_node cols node

let print_definition (define : tyenv) cols name (info : value_info) (value : MalValue) =
    let accu1 = List()
    let valvar = match info.vi_access with Immutable -> "val" | Mutable -> "var"
    accu1.Add(textNode (sprintf "%s %s :" valvar name))
    let ty_node = node_of_scheme define info.vi_type
    weld ty_node " ="
    accu1.Add(ty_node)
    let node1 = createSection Flow 0 (accu1.ToArray())

    let accu2 = List()
    accu2.Add(node1)
    let value =
        match info.vi_access with
        | Immutable -> value
        | Mutable -> (value :?> Value.MalVar).Content
    accu2.Add(node_of_value define info.vi_type value)
    let node2 = createSection Flow 1 (accu2.ToArray())
    update_sizes node2
    fst (string_of_node cols node2)

type lang =
    | En
    | Ja

let string_of_kind lang kind upper =
    match lang with
    | En ->
        let s =
            match kind with
            | Expression -> "Expression"
            | Pattern -> "Pattern"
            | Variable -> "Variable"
            | Constructor -> "Constructor"
            | Label -> "Label"
            | Record_expression -> "Record expression"
            | Function_name -> "Function name"
            | Type_parameter -> "Type parameter"
            | Variable_name -> "Variable name"
            | Function_definition -> "Function definition"
            | Variable_definition -> "Variable definition"
            | Type_definition -> "Type definition"
            | Type_name -> "Type name"
        if upper then s
        else s.ToLowerInvariant()
    | Ja ->
        match kind with
        | Expression -> "式"
        | Pattern -> "パターン"
        | Variable -> "変数"
        | Constructor -> "コンストラクタ"
        | Label -> "ラベル"
        | Record_expression -> "レコード式"
        | Function_name -> "関数名"
        | Type_parameter -> "型パラメータ"
        | Variable_name -> "変数名"
        | Function_definition -> "関数定義"
        | Variable_definition -> "変数定義"
        | Type_definition -> "型定義"
        | Type_name -> "型名"

let print_typechk_error lang cols desc =
    match lang with
    | En ->
        match desc with
        | Type_mismatch (tyenv, kind, ty1, ty2) ->
            let name_of_var = create_tvar_assoc_table()
            let accu = List()
            accu.Add(textNode (sprintf "%s has type" (string_of_kind lang kind true)))
            accu.Add(node_of_type tyenv name_of_var ty1)
            accu.Add(textNode "where")
            accu.Add(node_of_type tyenv name_of_var ty2)
            accu.Add(textNode "was expected.")
            let node = createSection Vertical 0 (accu.ToArray())
            update_sizes node
            fst (string_of_node cols node)
        | Multiple_occurence (kind, name, defkind) -> sprintf "%s %s occurs multiply in %s." (string_of_kind lang kind true) name (string_of_kind lang defkind false)
        | Constructor_undefined name -> sprintf "Variant %s is not defined." name
        | Constructor_requires_argument name -> sprintf "Variant %s requires argument." name
        | Constructor_takes_no_argument name -> sprintf "Variant %s takes no argument." name
        | Constructor_used_with_wrong_number_of_arguments (name, n, m) -> sprintf "Variant %s takes %d argument(s) but used with %d argument(s)." name n m
        | Label_undefined name -> sprintf "Undefined label %s." name
        | Label_undefined_for_type (label, type_name) -> sprintf "Label %s is not defined in type %s." label type_name
        | Unbound_identifier name -> sprintf "Unbound identifier %s." name
        | Binding_names_are_inconsistent -> "Types of bindings are inconsistent."
        | Binding_types_are_inconsistent -> "Names of bindings are inconsistent."
        | Unbound_type_variable name -> sprintf "Unbound type variable %s." name
        | Undefined_type_constructor name -> sprintf "Undefined type constructor %s." name
        | Wrong_arity_for_type name -> sprintf "Wrong arity for type %s." name
        | Type_definition_contains_immediate_cyclic_type_abbreviation -> "Type definition contains immediate cyclic type abbreviation."
        | Integer_literal_overflow -> "Integer literal overflow."
        | Some_labels_are_missing -> "Some labels are missing."
        | Multiple_arguments_to_constructor_must_be_tupled -> "Multiple arguments to constructor must be tupled."
        | This_expression_is_not_a_function -> "This expression is not a function."
        | Too_many_arguments_for_this_function -> "Too many arguments for this function."
        | Cannot_use_this_command_inside_an_expression -> "Cannot use this command inside an expression."
        | Cannot_use_when_clause_in_try_construct -> "Cannot use when clause in try construct."
        | Invalid_printf_format -> "Invalid printf format."
        | Not_mutable (kind, name) -> sprintf "%s %s is not mutable." (string_of_kind lang kind true) name
        | Invalid_identifier -> "Invalid identifier."
        | Invalid_type_name -> "Invalid type name."
        | Invalid_label_name -> "Invalid label name."
        | Invalid_constructor_name -> "Invalid constructor name."
        | This_expression_is_not_a_record -> "This expression is not a record."
        | Partially_applied -> "Beware, this function is partially applied."
        | Useless_with_clause -> "All the fields are explicitly listed in this record: the 'with' clause is useless."
        | Already_abstract name -> sprintf "Type %s is already abstract." name
        | Basic_types_cannot_be_hidden -> "Basic types cannot be hidden."
        | Lexer_created (name, count) -> sprintf "Lexer %s created. %d states." name count
        | Invalid_lexer_definition msg -> sprintf "Invalid lexer definition. %s." msg

    | Ja ->
        match desc with
        | Type_mismatch (tyenv, kind, ty1, ty2) ->
            let name_of_var = create_tvar_assoc_table()
            let accu = List()
            accu.Add(textNode (sprintf "この%sは" (string_of_kind lang kind true)))
            accu.Add(node_of_type tyenv name_of_var ty1)
            accu.Add(textNode "型ですが")
            accu.Add(node_of_type tyenv name_of_var ty2)
            accu.Add(textNode "型である必要があります。")
            let node = createSection Vertical 0 (accu.ToArray())
            update_sizes node
            fst (string_of_node cols node)
        | Multiple_occurence (kind, name, defkind) -> sprintf "%s %s が%s中で複数回使われています。" (string_of_kind lang kind true) name (string_of_kind lang defkind false)
        | Constructor_undefined name -> sprintf "コンストラクタ %s は未定義です。" name
        | Constructor_requires_argument name -> sprintf "コンストラクタ %s は引数が必要ですが、引数なしで使われています。" name
        | Constructor_takes_no_argument name -> sprintf "コンストラクタ %s は引数を取りませんが、引数とともに使われています。" name
        | Constructor_used_with_wrong_number_of_arguments (name, n, m) -> sprintf "コンストラクタ %s は%d個の引数を取りますが%d個の引数と共に使われています。" name n m
        | Label_undefined name -> sprintf "ラベル名 %s は未定義です。" name
        | Label_undefined_for_type (label, type_name) -> sprintf "型 %s について、ラベル名 %s は定義されていません。" type_name label
        | Unbound_identifier name -> sprintf "変数 %s は未定義です。" name
        | Binding_names_are_inconsistent -> "束縛変数の名前が一致しません。"
        | Binding_types_are_inconsistent -> "束縛変数の型が一致しません。"
        | Unbound_type_variable name -> sprintf "束縛されていない型変数 %s が使われています。" name
        | Undefined_type_constructor name -> sprintf "定義されていない型構築子 %s が使われています。" name
        | Wrong_arity_for_type name -> sprintf "多相型 %s へ与える型引数の個数が定義と異なります。" name
        | Type_definition_contains_immediate_cyclic_type_abbreviation -> "型定義が直接に再帰的な型略称を含んでいます。"
        | Integer_literal_overflow -> "整数リテラルの値が表現可能な範囲を超えています。"
        | Some_labels_are_missing -> "いくつかのラベルについて値が指定されていません。"
        | Multiple_arguments_to_constructor_must_be_tupled -> "コンストラクタへの複数の引数はタプルとして与える必要があります。"
        | This_expression_is_not_a_function -> "この式は関数ではありません。"
        | Too_many_arguments_for_this_function -> "関数に与える引数の数が多すぎます。"
        | Cannot_use_this_command_inside_an_expression -> "式の内部ではこのコマンドを使用できません。"
        | Cannot_use_when_clause_in_try_construct -> "try構文ではwhen節を使用できません。"
        | Invalid_printf_format -> "無効な printf フォーマット文字列です。"
        | Not_mutable (kind, name) -> sprintf "%s %s は変更可能ではありません。" (string_of_kind lang kind true) name
        | Invalid_identifier -> "識別子が非妥当です。"
        | Invalid_type_name -> "型名が非妥当です。"
        | Invalid_label_name -> "ラベル名が非妥当です。"
        | Invalid_constructor_name -> "コンストラクタ名が非妥当です。"
        | This_expression_is_not_a_record -> "この式はレコードではありません。"
        | Partially_applied -> "この式は部分適用されています。ご注意ください。"
        | Useless_with_clause -> "全てのフィールドが明示的に与えられているため、 with 節は不要です。"
        | Already_abstract name -> sprintf "型 %s は既に抽象型です。" name
        | Basic_types_cannot_be_hidden -> "基本型は隠蔽できません。"
        | Lexer_created (name, count) -> sprintf "レキサ %s が作成されました。状態数は %d です。" name count
        | Invalid_lexer_definition msg -> sprintf "レキサの定義が非妥当です。%s." msg
    
let print_message lang cols (msg : Message) =
    match msg with
    | LexicalError (err, loc) -> sprintf "> %s\r\n  Lexical error (%A).\r\n" (Syntax.describe_location loc) err
    | SyntaxError loc ->  sprintf "> %s\r\n  Syntax error.\r\n" (Syntax.describe_location loc)
    | TypeError (err, loc) ->
        let sb = StringBuilder()
        bprintf sb "> %s\r\n" (Syntax.describe_location loc)
        bprintf sb "%s\r\n" (print_typechk_error lang cols err)
        sb.ToString()
    | EvaluationComplete (tyenv, value, ty)->
        print_value tyenv cols ty value + "\r\n"
    | NewValues (tyenv, new_values) ->
        let sb = new StringBuilder()
        for name, value, info in new_values do
            sb.Add (print_definition tyenv cols name info value)
            sb.Add("\r\n")
        sb.ToString()
    | TypeDefined names ->
        let sb = StringBuilder()
        List.iter (fun name -> bprintf sb "Type %s defined.\r\n" name) names
        sb.ToString()
    | ExceptionDefined name -> sprintf "Exception %s is defined.\r\n" name
    | TypeHidden name -> sprintf "Type %s is now abstract.\r\n" name
    | ValueRemoved name -> sprintf "Value %s has been removed.\r\n" name
    | UncaughtException (tyenv, exn_value) ->
        let buf = StringBuilder()
        buf.Add("UncaughtException: ")
        buf.Add(print_value_without_type tyenv cols ty_exn exn_value)
        buf.AppendLine() |> ignore
        buf.ToString()
        //stacktrace 10 buf
    | UncatchableException msg -> sprintf "UncatchableException: %s.\r\n" msg
