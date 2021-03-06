﻿module FsMiniMAL.Typechk

open System
open System.Collections.Generic
open System.Collections.Immutable

open Syntax
open Types
open Unify

type kind =
    | Expression
    | Pattern
    | Variable
    | Constructor
    | Label
    | Record_expression
    | Variable_name
    | Function_name
    | Type_parameter
    | Function_definition
    | Variable_definition
    | Type_definition
    | Type_name

type type_error_desc =
    | Type_mismatch of tyenv * kind * type_expr * type_expr
    | Multiple_occurence of kind : kind * name : string * definition_kind : kind
    | Constructor_undefined of string
    | Constructor_requires_argument of string
    | Constructor_takes_no_argument of string
    | Constructor_used_with_wrong_number_of_arguments of name : string * expected : int * given : int
    | Label_undefined of string
    | Label_undefined_for_type of label : string * record_type_name : string
    | Unbound_identifier of string
    | Binding_names_are_inconsistent
    | Binding_types_are_inconsistent
    | Unbound_type_variable of string
    | Wrong_arity_for_type of string
    | Undefined_type_constructor of string
    | Type_definition_contains_immediate_cyclic_type_abbreviation
    | Integer_literal_overflow
    | Some_labels_are_missing
    | Multiple_arguments_to_constructor_must_be_tupled
    | This_expression_is_not_a_function
    | Too_many_arguments_for_this_function
    | Cannot_use_this_command_inside_an_expression
    | Cannot_use_when_clause_in_try_construct
    | Invalid_printf_format
    | Invalid_identifier
    | Invalid_type_name
    | Invalid_label_name
    | Invalid_constructor_name
    | Not_mutable of kind * string
    | This_expression_is_not_a_record
    | Already_abstract of string
    | Basic_types_cannot_be_hidden
    | Invalid_lexer_definition of string
    // Warnings
    | Partially_applied
    | Useless_with_clause
    | Lexer_created of name : string * state_count : int

type warning_sink = (type_error_desc * location) -> unit

exception Type_error of type_error_desc * location

/// Throws Type_error if name list contains duplicated name.
let rec all_differ loc kind definition_kind names =
    let set = HashSet<string>()
    for name in names do
        if set.Contains name then
            raise (Type_error (Multiple_occurence (kind, name, definition_kind), loc))
        else set.Add name |> ignore

let generic_level = -1

let new_tvar level = Tvar { link = None; level = level; }

let is_constructor (s : string) =
    let c = s.[0]
    match c with
    | 't' -> s = "true"
    | 'f' -> s = "false"
    | ':' -> s = "::"
    | '[' -> s = "[]"
    | '$' -> false
    | _ -> Char.IsUpper(c)

let rec get_pattern_name (pat : Syntax.pattern) =
    match pat.sp_desc with
    | SPid s when s <> "$" && (not (is_constructor s)) -> s
    | SPtype (p, _) -> get_pattern_name p
    | _ -> ""

let is_tvar (ty : type_expr) =
    match repr ty with
    | Tvar _ -> true
    | _ -> false

let option_repr (ty : type_expr option) =
    match ty with
    | None -> None
    | Some ty -> Some (repr ty)

/// Creates Types.type_expr from Syntax.type_expr.
/// When unknown type var was found in Syntax.type_expr, throws error or assign new type var depending on is_typedef.
let rec type_expr tyenv (level_for_new_tvar : int option) (type_vars : Dictionary<string, type_expr>) sty =
    match sty.st_desc with
    | STvar s ->
        match type_vars.TryGetValue s with
        | true, ty -> ty
        | false, _ ->
            match level_for_new_tvar with
            | None -> raise (Type_error (Unbound_type_variable s, sty.st_loc))
            | Some level ->
                let ty = new_tvar level
                type_vars.Add(s, ty)
                ty
    | STarrow (st1, st2) -> Tarrow ("", (type_expr tyenv level_for_new_tvar type_vars st1), (type_expr tyenv level_for_new_tvar type_vars st2))
    | STtuple stl -> Ttuple (List.map (type_expr tyenv level_for_new_tvar type_vars) stl)
    | STconstr (s, stl) ->
        match tyenv.types.TryFind s with
        | Some info ->
            if List.length stl <> List.length info.ti_params then
                raise (Type_error (Wrong_arity_for_type s, sty.st_loc))
            subst (List.zip info.ti_params (List.map (type_expr tyenv level_for_new_tvar type_vars) stl)) info.ti_res
        | None -> raise (Type_error (Undefined_type_constructor s, sty.st_loc))

/// Define new type from Syntax.typedef
let add_typedef tyenv loc dl =
    
    // Checks new type names are lowercase.
    for d in dl do
        if is_constructor d.sd_name then
            raise (Type_error (Invalid_type_name, d.sd_loc))
    
    // Checks duplicate in new type names.
    all_differ loc kind.Type_name kind.Type_definition (List.map (fun td -> td.sd_name) dl)

    // Check variant case names or record label names.
    for d in dl do
        match d.sd_kind with
        | SKrecord fields ->
            for name, _, _ in fields do
                if is_constructor name then
                    raise (Type_error (Invalid_label_name, d.sd_loc))

        | SKvariant cases ->
            for name, _ in cases do
                if not (Char.IsUpper name.[0]) then
                    raise (Type_error (Invalid_constructor_name, d.sd_loc))

        | _ -> ()
                
    // Checks duplicates in variant names.
    for d in dl do
        match d.sd_kind with
        | SKvariant cl ->
            let constructors = List.map fst cl
            all_differ d.sd_loc Constructor Type_definition constructors
        | _ -> ()
    
    // Checks duplicates in record labels.
    for d in dl do
        match d.sd_kind with
        | SKrecord fl ->
            let labels = List.map (fun (labels, _, _) -> labels) fl
            all_differ d.sd_loc Label Type_definition labels
        | _ -> ()
        
    // Checks duplicates in type var name.
    List.iter (fun td -> all_differ td.sd_loc kind.Type_parameter kind.Type_definition td.sd_params) dl

    // Checks there is no circular type abbreviation.
    let abbrev_defs = List.choose (function { Syntax.sd_kind = SKabbrev ty } as sd -> Some (sd, sd.sd_name, ty) | _ -> None) dl
    let abbrev_defs_map = Map.ofList (List.map (fun (_, name, ty) -> (name, ty)) abbrev_defs)
    let check_cyclic_abbrev (d : typedef) =
        let name, ty = match d.sd_kind with SKabbrev ty -> d.sd_name, ty | _ -> dontcare()
        let mutable visited = Set.ofArray [| name |]
        let rec visit (ty : Syntax.type_expr) =
            match ty.st_desc with
            | STvar _ -> ()
            | STarrow (ty1, ty2) -> visit ty1; visit ty2
            | STtuple l -> List.iter visit l
            | STconstr (name, args) ->
                if abbrev_defs_map.ContainsKey name then
                    if visited.Contains name then
                        raise (Type_error (Type_definition_contains_immediate_cyclic_type_abbreviation, loc))
                    else
                        visited <- Set.add name visited
                        visit abbrev_defs_map.[name]
                List.iter visit args
        visit ty
    List.iter (fun (def, _, _) -> check_cyclic_abbrev def) abbrev_defs

    // Create tyenv with dummy definitions.
    let dl, tyenv_with_dummy_defs = list_mapFold (fun tyenv td ->
        let id = type_id_new ()
        // Create type var instance for type parameters.
        let params' = List.map (fun _ -> { link = None; level = 0 }) td.sd_params
        // Set the dummy type to the global table.
        let ti = make_ti id td.sd_name params' Kbasic
        (td, id, ti), Types.add_type tyenv ti) tyenv dl

    /// Evaluate the type expressions syntax tree in environment with dummy types. 
    let dl =
        List.map (fun (td, id, ti_dummy) ->
            // Craete type information from syntax tree.
            // When type constructor is used in syntax tree, arity matching is checked.
            // For recursive type definitions, arity is checked using dummy type info.
            let type_vars = Dictionary<string, type_expr>()
            List.iter2 (fun name tv -> type_vars.Add(name, (Tvar tv))) td.sd_params ti_dummy.ti_params
            let kind = 
                match td.sd_kind with
                | SKabbrev sty -> Kabbrev (type_expr tyenv_with_dummy_defs None type_vars sty)
                | SKvariant cl -> Kvariant (List.mapi (fun i (s, stl) -> (s, i, (List.map (type_expr tyenv_with_dummy_defs None type_vars) stl))) cl)
                | SKrecord fl -> Krecord (List.map (fun (s, sty, access) -> (s, (type_expr tyenv_with_dummy_defs None type_vars sty), access)) fl)
            let ti = { ti_dummy with ti_kind = kind }
            (td, id, ti_dummy, ti)) dl
    
    // Create tyenv with real type information.
    List.fold (fun tyenv (_, _, _, ti) -> Types.add_type tyenv ti) tyenv dl

let hide_type (tyenv : tyenv) name loc =
        match tyenv.types.TryFind(name) with
        | Some info ->
            let id = match info.ti_res with Tconstr (id, _) -> id | _ -> dontcare()
            if id <= id_option then
                raise (Type_error (Basic_types_cannot_be_hidden, loc))
            match info.ti_kind with
            | Kbasic -> raise (Type_error (Already_abstract name, loc))
            | _ ->
                let tyenv = remove_type tyenv info
                let info = { info with ti_kind = Kbasic }
                add_type tyenv info
        | None -> raise (Type_error (Undefined_type_constructor name, loc))

/// Unifies ty with 'a -> 'b where 'a and 'b are new type variable at current_level. Then returns ('a, 'b).
/// If impossible, raises Unify.
let filter_arrow tyenv current_level ty = 
    match repr ty with
    | Tarrow(_, ty1, ty2) -> ty1, ty2 // fast path
    | ty -> 
        let ty1 = new_tvar current_level
        let ty2 = new_tvar current_level
        unify tyenv ty (Tarrow ("", ty1, ty2))
        ty1, ty2

/// Unifies ty with 'a -> 'b ... -> 'result where 'a, 'b, ... are new type variable at current_level.
/// Then returns ['a, 'b, ..., 'result] where length of resultant list is n + 1.
/// If impossible, raises Unify.
let filter_arrow_n tyenv current_level n ty =
    let rec loop n ty =
        if n = 0 then [ty]
        else
            let ty1, ty2 = filter_arrow tyenv current_level ty
            ty1 :: (loop (n - 1) ty2)
    loop n ty

let rec is_record (tyenv : tyenv) (some_ty : type_expr option) =
    match some_ty with
    | Some ty ->
        match expanded tyenv ty with
        | Tconstr (id, _) as ty ->
            match tyenv.types_of_id.TryGetValue(id) with
            | true, ti ->
                match ti.ti_kind with
                | Krecord _ -> Some id
                | _ -> None
            | _ -> dontcare()
        | _ -> None
    | _ -> None

/// Copy the type expression.
/// When type var at generic level is found, replace it with newly created type var at current level.
let instanciate_scheme current_level ty =
    let vars = ref [] in
    let rec inst ty =
        match repr ty with
        | Tvar tv when tv.level = generic_level ->
            match assqo tv !vars with
            | Some ty' -> ty'
            | None ->
                let ty' = new_tvar current_level
                vars := (tv, ty') :: !vars
                ty'
        | ty -> map_type inst ty
    inst ty

/// Take constr_info, and copy the argument type expression and resultant type expression.
/// When type var is found, replace it with newly created type var at current level.
let instanciate_constr current_level info = 
    let s = List.map (fun tv -> (tv, new_tvar current_level)) info.ci_params
    let args = List.map (subst s) info.ci_args
    let res = subst s info.ci_res
    args, res

/// Take label_info, and copy the field type expression and resultant type expression.
/// When type var is found, replace it with newly created type var at current level.
let instanciate_label current_level info =
    let s = List.map (fun tv -> (tv, new_tvar current_level)) info.li_params
    let ty_field = subst s info.li_arg
    let ty_rec = subst s info.li_res
    ty_field, ty_rec

let instanciate_record tyenv current_level id_record =
    let info = tyenv.types_of_id.[id_record]
    let s = List.map (fun tv -> (tv, new_tvar current_level)) info.ti_params
    let record_fields = List.mapi (fun i (name, ty, access) -> (name, (i, subst s ty, access))) (match info.ti_kind with Krecord l -> l | _ -> dontcare())
    let ty_res = subst s info.ti_res
    record_fields, ty_res

/// Returns true if all objects which will be created in evaluation of this expression are immutable and there is no side-effects to external objects.
let rec is_nonexpansive (e : Syntax.expression) =
    match e.se_desc with
    | SEid _ | SEint _ | SEchar _ | SEfloat _ | SEstring _ | SEformat _ | SEfn _ -> true
    | SElist (LKarray, _) | SEapply _ | SEset _ -> false
    | SEconstr (_, l) -> List.forall is_nonexpansive l
    | SEtuple l | SElist (LKlist, l) -> List.forall is_nonexpansive l
    | SEurecord (orig, fields) ->
        (match orig with None -> true | Some e -> is_nonexpansive e) &&
        List.forall (fun (_, access, e) -> access = Immutable && is_nonexpansive e) fields
    | SEbegin l -> List.forall cmd_nonexpansive l
    | SEcase (e, cases) ->
        is_nonexpansive e &&
        List.forall (fun (_, ew, e) -> (match ew with Some ew -> is_nonexpansive ew | None -> true) && is_nonexpansive e) cases
    | SEtry (e, cases) ->
        is_nonexpansive e &&
        List.forall (fun (_, _, e) -> is_nonexpansive e) cases
    | SEifthenelse (e1, e2, e3) ->
        is_nonexpansive e1 &&
        is_nonexpansive e2 &&
        (match e3 with Some e3 -> is_nonexpansive e3 | None -> true)
    | SEfor (_, e1, _, e2, e3) -> is_nonexpansive e1 && is_nonexpansive e2 && is_nonexpansive e3
    | SEwhile (e1, e2) -> is_nonexpansive e1 && is_nonexpansive e2
    | SEtype (e, _) -> is_nonexpansive e
    | SErecord _ 
    | SEsetfield _ 
    | SEgetfield _ -> dontcare()

/// Returns true if all objects created in execution of commands are immutable.
and cmd_nonexpansive cmd =
    match cmd.sc_desc with
    | SCexpr e -> is_nonexpansive e
    | SCval l -> List.forall (fun (_, e) -> is_nonexpansive e) l
    | SCfun _ -> true
    | SCvar _ -> false
    | _ -> dontcare()

let type_printf_cmds cmds ty_result = 
    let rec loop = 
        function 
        | PrintfFormat.PrintfCommand.Text _ :: t -> loop t
        | PrintfFormat.PrintfCommand.Spec { TypeChar = 's' } :: t -> Tarrow ("", ty_string, loop t)
        | PrintfFormat.PrintfCommand.Spec { TypeChar = ('d' | 'x' | 'X') } :: t -> Tarrow ("", ty_int, loop t)
        | PrintfFormat.PrintfCommand.Spec { TypeChar = ('f' | 'e' | 'E' | 'g' | 'G' | 'r') } :: t -> Tarrow ("", ty_float, loop t)
        | [] -> ty_result
        | _ -> raise PrintfFormat.InvalidFormatString
    loop cmds

/// Unify two types. If failed, throws type error for the pattern.
let unify_pat tyenv pat ty1 ty2 = 
    try unify tyenv ty1 ty2
    with Unify -> raise (Type_error (Type_mismatch (tyenv, Pattern, ty1, ty2), pat.sp_loc))

/// Unify two types. If failed, throws type error.
let unify_exp tyenv exp ty ty_expected =
    try unify tyenv ty ty_expected
    with Unify -> raise (Type_error (Type_mismatch (tyenv, Expression, ty, ty_expected), exp.se_loc))

/// Set genelic level to type vars with level > current_level.
let rec generalize current_level ty = 
    match repr ty with
    | Tvar tv -> 
        if tv.level > current_level then
            tv.level <- generic_level
    | ty -> do_type (generalize current_level) ty

/// Set current level to type vars with level > current level.
let rec make_nongen current_level ty = 
    match repr ty with
    | Tvar tv -> 
        if tv.level > current_level then
            tv.level <- current_level
    | ty -> do_type (make_nongen current_level) ty

/// Pick the type information of variant type from constructor name.
/// If expected type is variant type and that type have definition for the used constructor name,
/// pick that type even if the constructor name is shadowed.
let pick_constr (tyenv : tyenv) (ty_hint : type_expr option) (name : string) =
    match tyenv.constructors.FindAll name with
    | [] -> None
    | constrs ->
        let ci_from_ty_hint =
            match option_repr ty_hint with
            | Some (Tconstr (id, _)) ->
                List.tryFind (function { ci_res = Tconstr (id', _) } -> id = id' | _ -> false) constrs
            | _ -> None
        match ci_from_ty_hint with
        | Some _ -> ci_from_ty_hint
        | None -> Some (List.head constrs)

type Typechecker(warning_sink : warning_sink, tyenv : tyenv, current_level : int) =
    let type_vars = Dictionary<string, type_expr>()
    let mutable tyenv = tyenv
    let mutable current_level = current_level

    let rec pattern (ty_hint : type_expr option) (pat : Syntax.pattern) =
        match pat.sp_desc with
        | SPid s ->
            if is_constructor s then
                match pick_constr tyenv ty_hint s with
                | None -> raise (Type_error(Constructor_undefined s, pat.sp_loc))
                | Some info ->
                    // If no-argument constr is used with argument, throw type error.
                    if not (List.isEmpty info.ci_args) then
                        raise (Type_error(Constructor_requires_argument s, pat.sp_loc))
                    let (_, ty_res) = instanciate_constr current_level info
                    pat.sp_desc <- SPblock (info.ci_tag, [])
                    (ty_res, [])
            else
                // This is identifier, not variant.
                let ty = new_tvar current_level
                (ty, [ (s, { vi_type = ty; vi_access = access.Immutable; }) ])
        | SPas (p, ident) ->
            if is_constructor ident then
                raise (Type_error (Invalid_identifier, pat.sp_loc))
            let ty, bnds = pattern ty_hint p
            let bnds = if mem_assoc ident bnds then remove_assoc ident bnds else bnds
            let bnds = (ident, { vi_type = ty; vi_access = access.Immutable; }) :: bnds
            (ty, bnds)
        | SPint _ -> ty_int, []
        | SPchar _ -> ty_char, []
        | SPfloat _ -> ty_float, []
        | SPstring _ -> ty_string, []
        | SPtuple l ->
            let tyl_hint =
                match option_repr ty_hint with
                | Some (Ttuple tyl) when l.Length = tyl.Length -> List.map (fun ty -> Some ty) tyl
                | _ -> List.init l.Length (fun _ -> None)
            let tyl, bnds = pattern_list pat.sp_loc tyl_hint l
            (Ttuple tyl, bnds)
        | SPlist (kind, l) ->
            let ty_item_hint =
                match kind, option_repr ty_hint with
                | LKlist,  Some (Tconstr (type_id.LIST, [ty_arg]))
                | LKarray, Some (Tconstr (type_id.ARRAY, [ty_arg])) -> Some ty_arg
                | _ -> None
            let tyl_item_hint = List.init l.Length (fun _ -> ty_item_hint)
            let tyl, bnds = pattern_list pat.sp_loc tyl_item_hint l
            // Item expressions in list pattern must be same. So unify them.
            let ty_accu = new_tvar current_level
            List.iter2 (fun pat ty -> unify_pat tyenv pat ty ty_accu) l tyl
            match kind with
            | LKlist -> Tconstr (type_id.LIST, [ ty_accu ]), bnds
            | LKarray -> Tconstr (type_id.ARRAY, [ ty_accu ]), bnds
        | SPapply (s, arg) ->
            if not (is_constructor s) then
                raise (Type_error(Invalid_constructor_name, pat.sp_loc))
            match pick_constr tyenv ty_hint s with
            | Some info ->
                // Create new type expression using constructor information.
                let ty_args, ty_res = instanciate_constr current_level info
                let ty_res_id = match info.ci_res with Tconstr (id, _) -> id | _ -> dontcare ()
                match ty_args, arg with
                | [], _ ->
                    raise (Type_error(Constructor_takes_no_argument s, pat.sp_loc))
                | [ty_arg], _ ->
                    // This variant takes one argument. Therefore if argument is syntactically tuple,
                    // it is taking single argument in tuple type.
                    let ty_arg_hint =
                        match option_repr ty_hint with
                        | Some (Tconstr (id, [ty_arg_hint])) when id = ty_res_id -> Some ty_arg_hint
                        | _ -> None
                    let ty_pat, bnds = pattern ty_arg_hint arg
                    unify_pat tyenv arg ty_pat ty_arg
                    pat.sp_desc <- SPblock (info.ci_tag, [arg])
                    (ty_res, bnds)
                | _, { sp_desc = SPtuple args } ->
                    // This variant takes multiple argument, so the argument must be syntactically tuple.
                    if args.Length = ty_args.Length then
                        let ty_args_hint =
                            match option_repr ty_hint with
                            | Some (Tconstr (id, tyl)) when id = ty_res_id && ty_args.Length = tyl.Length ->
                                List.map (fun ty -> Some ty) tyl
                            | _ -> List.init ty_args.Length (fun _ -> None)
                        let ty_pats, bnds = pattern_list pat.sp_loc ty_args_hint args
                        do_list3 (unify_pat tyenv) args ty_pats ty_args
                        pat.sp_desc <- SPblock (info.ci_tag, args)
                        (ty_res, bnds)
                    else raise (Type_error (Constructor_used_with_wrong_number_of_arguments (s, List.length ty_args, List.length args), pat.sp_loc))
                | _ -> raise (Type_error (Constructor_used_with_wrong_number_of_arguments (s, List.length ty_args, 1), pat.sp_loc))
            | None -> raise (Type_error (Constructor_undefined s, pat.sp_loc))
        | SPblock _ -> dontcare()
        | SPrecord l ->
            // Try to find record type from ty_hint
            let id_record = is_record tyenv ty_hint

            // If record type is still unknown, decide based on firstly seen record label.
            let id_record =
                match id_record with
                | Some id -> id
                | None ->
                    let first_lab, _ = List.head l
                    match tyenv.labels.TryFind first_lab with
                    | Some info -> info.li_id
                    | None -> raise (Type_error (Label_undefined first_lab, pat.sp_loc))
        
            let fields, ty_res = instanciate_record tyenv current_level id_record

            let fields =
                List.map (fun (lab, p) ->
                    match tryAssoc lab fields with
                    | Some (idx, ty, access) -> (lab, p, idx, ty, access)
                    | None -> raise (Type_error (Label_undefined_for_type (lab, tyenv.types_of_id.[id_record].ti_name), p.sp_loc))) l
        
            // type argument expressions
            let pl = List.map (fun (_, pat, _, _, _) -> pat) fields
            let tyl_hint = List.map (fun (_, _, _, ty, _) -> Some ty) fields
            let ty_args, bnds = pattern_list pat.sp_loc tyl_hint pl

            // unify record field type and argument type
            List.iter2 (fun (_, pat, _, ty_field, _) ty_arg -> unify_pat tyenv pat ty_arg ty_field) fields ty_args

            let record_len = match tyenv.types_of_id.[id_record].ti_kind with | Krecord l -> List.length l | _ -> dontcare ()

            // translate record to untyped block pattern
            let ary = Array.create record_len { sp_desc = SPany; sp_loc = pat.sp_loc }
            List.iter (fun (_, pat, i, _, _) -> ary.[i] <- pat) fields
            pat.sp_desc <- SPblock (0, List.ofArray ary)
            (ty_res, bnds)
        | SPany -> (new_tvar current_level, [])
        | SPtype (pat, sty) ->
            let ty_res = type_expr tyenv (Some 1) type_vars sty
            let ty_pat, bnds = pattern (Some ty_res) pat
            unify_pat tyenv pat ty_pat ty_res
            (ty_res, bnds)
        | SPor (a, b) ->
            let ty_a, bnds_a = pattern (ty_hint) a
            let ty_b, bnds_b = pattern (ty_hint) b
            let sorted_names bnds = Array.sort (Array.map fst (Array.ofList bnds))
            if sorted_names bnds_a <> sorted_names bnds_b then
                raise (Type_error (Binding_names_are_inconsistent, pat.sp_loc))
            for name, vi1 in bnds_a do
                let vi2 = (Misc.assoc name bnds_b)
                try unify tyenv vi1.vi_type vi2.vi_type
                with Unify -> raise (Type_error (Binding_types_are_inconsistent, pat.sp_loc))
            unify_pat tyenv b ty_b ty_a
            (ty_a, bnds_a)

    /// Type list of patterns. If there was duplicated name, throw type error.
    and pattern_list loc (tyl_hint : type_expr option list) (patl : Syntax.pattern list) =
        let tyl, bndss = List.unzip (List.map2 (fun ty_hint pat -> pattern ty_hint pat) tyl_hint patl)
        let bnds =
            List.foldBack (fun bnd bnds ->
                (List.iter (fun (s, _) ->
                    if mem_assoc s bnds then
                        raise (Type_error ((Multiple_occurence (kind.Variable_name , s, kind.Pattern)), loc))) bnd;
                bnd @ bnds)) bndss []
        (tyl, bnds)

    let rec expression (ty_hint : type_expr option) (e : expression) =
      let ty =
        match e.se_desc with
        | SEid s ->
            if is_constructor s then
                match tyenv.constructors.TryFind(s) with
                | Some info ->
                    if not (List.isEmpty info.ci_args) then
                        raise (Type_error(Constructor_requires_argument s, e.se_loc))
                    let _, ty_res = instanciate_constr current_level info
                    e.se_desc <- SEconstr (info.ci_tag, [])
                    ty_res
                | None -> raise (Type_error (Constructor_undefined s, e.se_loc))
            else
                match try_get_value tyenv s with
                | Some info -> instanciate_scheme current_level info.vi_type
                | None -> raise (Type_error (Unbound_identifier s, e.se_loc))
        | SEint s ->
            try int s |> ignore
            with :? System.OverflowException -> raise (Type_error (Integer_literal_overflow, e.se_loc))
            ty_int
        | SEchar _ -> ty_char
        | SEfloat _ -> ty_float
        | SEtuple el ->
            let tyl_hint =
                match option_repr ty_hint with
                | Some (Ttuple tyl) when tyl.Length = el.Length -> List.map (fun ty -> Some ty) tyl
                | _ -> List.init el.Length (fun _ -> None)
            Ttuple (List.map2 expression tyl_hint el)
        | SElist (kind, el) ->
            let ty_item_hint =
                match option_repr ty_hint with
                | Some (Tconstr (type_id.ARRAY, [ty_arg])) -> Some ty_arg
                | _ -> None
            let tyl_items = List.map (expression ty_item_hint) el
            let ty_accu = new_tvar current_level
            List.iter2 (fun e ty -> unify_exp tyenv e ty ty_accu) el tyl_items
            let id = match kind with LKlist -> type_id.LIST | LKarray -> type_id.ARRAY
            Tconstr (id, [ ty_accu ])
        | SEstring s ->
            match option_repr ty_hint with
            | Some (Tconstr (type_id.FORMAT, _)) ->
                let cmds =
                    try PrintfFormat.parse_fmt s
                    with PrintfFormat.InvalidFormatString -> raise (Type_error (Invalid_printf_format, e.se_loc))
                e.se_desc <- SEformat cmds
                let ty_res = new_tvar current_level
                let ty_args = type_printf_cmds cmds ty_res
                Tconstr (type_id.FORMAT, [ty_args; ty_res])
            | _ -> ty_string
        | SErecord (orig, fields) ->

            // if there is duplicate in labels, report type error
            all_differ e.se_loc kind.Label kind.Record_expression (List.map fst fields)

            // infer orig of { orig with ... }.
            let ty_orig =
                Option.bind (fun e ->
                    let ty = expression ty_hint e
                    // if orig is available but is neither of record nor tvar, report type error
                    if not ((is_record tyenv (Some ty)).IsSome || is_tvar ty) then
                        raise (Type_error (This_expression_is_not_a_record, e.se_loc))
                    Some ty) orig
        
            // get record type id from orig if possible 
            let id_record = is_record tyenv ty_orig

            // if record type id is still not found, and recoed type is given in ty_hint, use it.   
            let id_record =
                match id_record with
                | Some _ -> id_record
                | None -> is_record tyenv ty_hint
        
            // if record type is still not found, decide based on label name of firstly given field
            let id_record =
                match id_record with
                | Some id -> id
                | None ->
                    let lab = fst (List.head fields)
                    match tyenv.labels.TryFind(lab) with
                    | Some info -> info.li_id
                    | None -> raise (Type_error (Label_undefined lab, e.se_loc))

            // instanciate record type
            let record_fields, ty_res = instanciate_record tyenv current_level id_record

            // narrow ty_res according to ty_orig. this will not fail.
            Option.iter (fun ty_orig -> unify tyenv ty_orig ty_res) ty_orig
        
            // bind given field types to field expressions
            let fields =
                List.map (fun (lab, e) ->
                    match tryAssoc lab record_fields with
                    | Some (idx, ty, access) ->
                        (lab, idx, ty, access, e)
                    | None -> raise (Type_error (Label_undefined_for_type (lab, tyenv.types_of_id.[id_record].ti_name), e.se_loc))) fields
        
            // tests for number of given fields
            match orig, List.length fields = List.length record_fields with
            | None, false ->
                raise (Type_error (Some_labels_are_missing, e.se_loc))
            | Some _, true ->
                warning_sink (Useless_with_clause, e.se_loc)
            | _ -> ()

            // typecheck for field expressions
            List.iter (fun (_, _, ty_field, _, e) -> expression_expect ty_field e) fields

            e.se_desc <- SEurecord (orig, (List.map (fun (_, idx, _, access, e) -> (idx, access, e)) fields))
            ty_res
        | SEapply (func, args) ->
            let apply_default() =
                let ty1 = expression None func
                try filter_arrow tyenv current_level ty1 |> ignore
                with Unify -> raise (Type_error (This_expression_is_not_a_function, func.se_loc))
                let ty_args, ty_res =
                    try split_last (filter_arrow_n tyenv current_level args.Length ty1)
                    with Unify -> raise (Type_error (Too_many_arguments_for_this_function, func.se_loc))
                List.iter2 (fun ty e -> expression_expect ty e) ty_args args
                ty_res
            match func with
            | { se_desc = SEid s } ->
                if is_constructor s then
                    match pick_constr tyenv ty_hint s with
                    | None -> raise (Type_error (Constructor_undefined s, func.se_loc))
                    | Some info ->
                        let ty_args, ty_res = instanciate_constr current_level info
                        let e_args =
                            match args with
                            | [e] -> e
                            | [] -> dontcare()
                            | _ -> raise (Type_error (Multiple_arguments_to_constructor_must_be_tupled, func.se_loc))
                        match ty_args, e_args with
                        | [], _ -> raise (Type_error(Constructor_takes_no_argument s, func.se_loc))
                        | [ty_arg], _ ->
                            expression_expect ty_arg e_args
                            e.se_desc <- SEconstr (info.ci_tag, [e_args])
                            ty_res
                        | _, { se_desc = SEtuple el } ->
                            if List.length el = List.length ty_args then
                                List.iter2 expression_expect ty_args el
                                e.se_desc <- SEconstr (info.ci_tag, el)
                                ty_res
                            else raise (Type_error(Constructor_used_with_wrong_number_of_arguments (s, ty_args.Length, el.Length), func.se_loc))
                        | _, _ -> raise (Type_error(Constructor_used_with_wrong_number_of_arguments (s, ty_args.Length, 1), func.se_loc))
                else
                    match s.[0] with
                    | '+' | '-' | '*' | '/' ->
                        let args_len = args.Length
                        if s.Length = 1 && args_len <= 2 then
                            let tyl_args = List.map (fun e -> expression None e) args
                            let mutable float_count = 0
                            let mutable tvar_count = 0
                            for ty in tyl_args do
                                if same_type tyenv ty ty_float then
                                    float_count <- float_count + 1
                                elif is_tvar ty then
                                    tvar_count <- tvar_count + 1
                            if float_count > 0 && (float_count + tvar_count) = args_len then
                                List.iter2 (fun e_arg ty_arg -> unify_exp tyenv e_arg ty_arg ty_float) args tyl_args
                                func.se_desc <- SEid (s + ".")
                                if args_len = 2 then ty_float else ty_ff
                            else
                                List.iter2 (fun e_arg ty_arg -> unify_exp tyenv e_arg ty_arg ty_int) args tyl_args
                                if args_len = 2 then ty_int else ty_ii
                        else apply_default()
                    | '~' ->
                        let args_len = args.Length
                        if s = "~-" && args_len = 1 then
                            let ty_arg = expression None args.[0]
                            if same_type tyenv ty_arg ty_float then
                                unify_exp tyenv args.[0] ty_arg ty_float
                                func.se_desc <- SEid ("~-.")
                                ty_float
                            else
                                unify_exp tyenv args.[0] ty_arg ty_int
                                ty_int
                        else apply_default()
                    | '.' ->
                        if s = ".[]" then
                            match args with
                            | [arg0; arg1] ->
                                let ty_arg0 = expression None arg0
                                let ty_result =
                                    if same_type tyenv ty_arg0 ty_string then
                                        ty_char
                                    else
                                        let ty_item = new_tvar current_level
                                        let ty_array = Tconstr (type_id.ARRAY, [ty_item])
                                        unify_exp tyenv arg0 ty_arg0 ty_array
                                        ty_item
                                expression_expect ty_int arg1
                                ty_result
                            | _ -> dontcare()
                        else apply_default()
                    | '^' ->
                        let args_len = args.Length
                        if s.Length = 1 && args_len <= 2 then
                            let tyl_args = List.map (fun e -> expression None e) args
                            let mutable string_count = 0
                            let mutable tvar_count = 0
                            for ty in tyl_args do
                                if same_type tyenv ty ty_string then
                                    string_count <- string_count + 1
                                elif is_tvar ty then
                                    tvar_count <- tvar_count + 1
                            if string_count > 0 && (string_count + tvar_count) = args_len then
                                List.iter2 (fun e_arg ty_arg -> unify_exp tyenv e_arg ty_arg ty_string) args tyl_args
                                func.se_desc <- SEid "^^"
                                if args_len = 2 then ty_string else arrow ty_string ty_string
                            else
                                let ty_array = Tconstr (type_id.ARRAY, [new_tvar current_level])
                                List.iter2 (fun e_arg ty_arg -> unify_exp tyenv e_arg ty_arg ty_array) args tyl_args
                                if args_len = 2 then ty_array else arrow ty_array ty_array
                        else apply_default()
                    | '&' | '|' ->
                        let args_len = args.Length
                        if (s = "&&&" || s = "|||") && (args_len = 1 || args_len = 2) then
                            let tyl_args = List.map (fun e -> expression None e) args

                            let get_kind ty =
                                match repr ty with
                                | Tconstr (type_id.INT, _) -> 0 // int
                                | _ ->
                                    if same_type tyenv ty_int ty then
                                        1 // enum
                                    else
                                        -1 // other

                            match List.map get_kind tyl_args with
                            | [0; 0] -> ty_int
                            | [1; 0]
                            | [1; 1] -> tyl_args.[0]
                            | [0; 1] -> tyl_args.[1]
                            | [0] -> ty_ii
                            | [1] -> arrow tyl_args.[0] tyl_args.[0]
                            | _ ->
                                List.iter2 (fun e ty -> unify_exp tyenv e ty ty_int) args tyl_args
                                match args_len with
                                | 2 -> ty_int
                                | 1 -> ty_ii
                                | _ -> dontcare()
                        else apply_default()
                    | _ -> apply_default()
            | _ -> apply_default()
        | SEfn (patl, e1) ->
            let loc_patl = { (List.head patl).sp_loc with ed = (list_last patl).sp_loc.ed }
            let ty_args, new_bnds = pattern_list loc_patl (List.init patl.Length (fun _ -> None)) patl
            let names = List.map get_pattern_name patl
            let oldTyenv = tyenv
            tyenv <- add_values oldTyenv new_bnds
            let ty_res = expression None e1
            tyenv <- oldTyenv
            List.foldBack2 (fun name ty1 ty2 -> Tarrow (name, ty1, ty2)) names ty_args ty_res
        | SEbegin cl ->
            let cl', res = split_last cl
            let oldTyenv = tyenv
            List.iter (fun c ->
                let new_bnds = command c
                tyenv <- Types.add_values tyenv new_bnds) cl'
            let tyResult =
                match res.sc_desc with
                | SCexpr e1 ->
                    expression ty_hint e1
                | _ ->
                    let new_bnds = command res
                    tyenv <- Types.add_values tyenv new_bnds
                    ty_unit
            tyenv <- oldTyenv
            tyResult
        | SEcase (e, cases) ->
            let ty_arg = expression None e
            let ty_res = new_tvar current_level
            List.iter (fun (pat, ew, e) ->
                let ty_pat, new_values = pattern (Some ty_arg) pat
                unify_pat tyenv pat ty_pat ty_arg
                let oldTyenv = tyenv
                tyenv <- add_values tyenv new_values
                Option.iter (fun ew -> expression_expect ty_bool ew) ew
                expression_expect ty_res e
                tyenv <- oldTyenv) cases
            ty_res
        | SEtry (e, cases) ->
            if not (List.forall (function (_, None, _) -> true | (_, Some _, _) -> false) cases) then raise (Type_error (Cannot_use_when_clause_in_try_construct, e.se_loc))
            let ty_arg = expression None e
            List.iter (fun (pat, _, e) ->
                let ty_pat, new_values = pattern None pat
                unify_pat tyenv pat ty_pat ty_exn
                let oldTyenv = tyenv
                tyenv <- add_values tyenv new_values
                expression_expect ty_arg e
                tyenv <- oldTyenv) cases
            ty_arg
        | SEifthenelse (e1, e2, e3) ->
            expression_expect ty_bool e1
            match e3 with
            | Some e3 ->
                let ty_res = expression None e2
                expression_expect ty_res e3
                ty_res
            | None ->
                expression_expect ty_unit e2
                ty_unit
        | SEset (s, e1) ->
            match try_get_value tyenv s with
            | None -> raise (Type_error (Unbound_identifier s, e.se_loc))
            | Some info ->
                if info.vi_access = access.Immutable then
                    raise (Type_error (Not_mutable (Variable, s), e.se_loc))
                expression_expect info.vi_type e1;
                Ttuple []
        | SEgetfield (e1, s) ->
            let ty1 = expression None e1
            let candidates = tyenv.labels.FindAll s
            let info_from_ty1 =
                match repr ty1 with
                | Tconstr (id, _) -> List.tryFind (fun (info : label_info) -> info.li_id = id) candidates
                | _ -> None
            let info =
                match info_from_ty1, candidates with
                | Some info, _ -> info
                | None, hd :: _ -> hd
                | None, [] -> raise (Type_error (Label_undefined s, e.se_loc))
            let ty_field, ty_record = instanciate_label current_level info
            unify_exp tyenv e1 ty1 ty_record 
            e.se_desc <- SEapply ({ se_desc = SEid "."; se_loc = e.se_loc; se_type = None }, [e1; { se_desc = SEint (info.li_index.ToString()); se_loc = e.se_loc; se_type = None }])
            ty_field
        | SEsetfield (e1, s, e2) ->
            let ty1 = expression None e1
            let candidates = tyenv.labels.FindAll s
            let info_from_ty1 =
                match repr ty1 with
                | Tconstr (id, _) -> List.tryFind (fun (info : label_info) -> info.li_id = id) candidates
                | _ -> None
            let info =
                match info_from_ty1, candidates with
                | Some info, _ -> info
                | None, hd :: _ -> hd
                | None, [] -> raise (Type_error (Label_undefined s, e.se_loc))
            if info.li_access <> access.Mutable then
                raise (Type_error (Not_mutable (Label, s), e.se_loc))
            let ty_field, ty_record = instanciate_label current_level info
            unify_exp tyenv e1 ty1 ty_record 
            expression_expect ty_field e2
            e.se_desc <- SEapply ({ se_desc = SEid ".<-"; se_loc = e.se_loc; se_type = None }, [e1; { se_desc = SEint (info.li_index.ToString()); se_loc = e.se_loc; se_type = None }; e2])
            ty_unit
        | SEfor (s, e1, _, e2, e3) ->
            expression_expect ty_int e1
            expression_expect ty_int e2
            let oldTyenv = tyenv
            tyenv <- add_value tyenv s { vi_type = ty_int; vi_access = access.Immutable }
            statement e3 |> ignore
            tyenv <- oldTyenv
            Ttuple []
        | SEwhile (e1, e2) ->
            expression_expect ty_bool e1
            statement e2 |> ignore
            Ttuple []
        | SEtype (e1, sty) ->
            let ty = type_expr tyenv (Some 1) type_vars sty
            expression_expect ty e1
            ty
        | _ -> dontcare()
      e.se_type <- Some ty
      ty

    /// Infer the type of expression with expectation. The expectation is used as a hint.
    /// Returns unit and the result remains in the ty_expected.
    /// Throws type error if failed.
    and expression_expect ty_expected e =
        let ty = expression (Some ty_expected) e
        unify_exp tyenv e ty ty_expected

    and statement e =
        let ty = expression None e
        match repr ty with
        | Tarrow _ ->
            warning_sink (Partially_applied, e.se_loc)
        | _ -> ()
        ty

    and command (cmd : Syntax.command) =
        match cmd.sc_desc with
        | SCexpr e ->
            statement e |> ignore
            []
        | SCval l ->
            current_level <- current_level + 1
            let ty_patl, new_bnds = pattern_list cmd.sc_loc (List.init l.Length (fun _ -> None)) (List.map fst l)
            let l = List.map2 (fun (_, e) ty_pat -> (ty_pat, e)) l ty_patl
            List.iter (fun (ty_pat, e) -> expression_expect ty_pat e) l
            current_level <- current_level - 1
            List.iter (fun (ty_pat, e) ->
                if not (is_nonexpansive e) then
                    make_nongen current_level ty_pat) l
            List.iter (fun (ty_pat, _) -> generalize current_level ty_pat) l
            new_bnds
        | SCfun defs ->
            let names = List.map fst defs
            List.iter (fun name -> if is_constructor name then raise (Type_error (Invalid_identifier, cmd.sc_loc))) names
            all_differ cmd.sc_loc kind.Function_name kind.Function_definition names

            let defs = List.map (fun (name, expr) ->
                let dummy_info = { vi_type = new_tvar (current_level + 1); vi_access = access.Immutable; }
                (name, expr, dummy_info)) defs
            let oldTyenv = tyenv
            let tyenv_with_dummy_info = add_values tyenv (List.map (fun (name, _, info) -> (name, info)) defs)
            tyenv <- tyenv_with_dummy_info
            current_level <- current_level + 1
            let new_values =
                List.map (fun (name, expr, info) ->
                    let ty = expression None expr
                    unify_exp tyenv expr ty info.vi_type
                    let info = { vi_type = ty; vi_access = access.Immutable; }
                    name, info) defs
            current_level <- current_level - 1
            List.iter (fun (_, info) -> generalize current_level info.vi_type) new_values
            tyenv <- oldTyenv
            new_values
        | SCvar l ->
            let names = List.map fst l
            List.iter (fun (name : string) -> if is_constructor name then raise (Type_error (Invalid_identifier, cmd.sc_loc))) names
            all_differ cmd.sc_loc kind.Variable_name kind.Variable_definition names

            List.map (fun (s, e) ->
                (s, { vi_type = expression None e; vi_access = access.Mutable })) l
        | SCtype _
        | SChide _
        | SCremove _
        | SCexn _
        | SClex _ -> raise (Type_error (Cannot_use_this_command_inside_an_expression, cmd.sc_loc))
        | _ -> dontcare()

    member this.Expression = expression
    member this.Command = command

let type_expression warning_sink tyenv (e : Syntax.expression) =
    let ty = Typechecker(warning_sink, tyenv, 1).Expression None e
    if is_nonexpansive e then generalize 0 ty
    ty

let type_command warning_sink tyenv (cmd : Syntax.command) =
    Typechecker(warning_sink, tyenv, 0).Command cmd

let tyenv_clone (tyenv : tyenv) =
    let tvars = Dictionary<type_var, type_var>(Misc.PhysicalEqualityComparer)

    let tvar_loop (orig : type_var) =
        match tvars.TryGetValue(orig) with
        | true, clone -> clone
        | false, _ ->
            let clone = { type_var.level = orig.level; link = None }
            tvars.[orig] <- clone
            clone
    
    let rec ty_loop (ty : type_expr) =
        match repr ty with
        | Tvar tv ->
            if tv.level = generic_level
            then ty
            else Tvar (tvar_loop tv)
        | Tarrow (name, ty1, ty2) ->
            let ty1_clone = ty_loop ty1
            let ty2_clone = ty_loop ty2
            if LanguagePrimitives.PhysicalEquality ty1 ty1_clone && LanguagePrimitives.PhysicalEquality ty2 ty2_clone then
                ty
            else
                Tarrow (name, ty1_clone, ty2_clone)
        | Ttuple tyl ->
            let tyl_clone = List.map ty_loop tyl
            if List.forall2 LanguagePrimitives.PhysicalEquality tyl tyl_clone
            then ty
            else Ttuple tyl_clone
        | Tconstr (id, tyl) ->
            let tyl_clone = List.map ty_loop tyl
            if List.forall2 LanguagePrimitives.PhysicalEquality tyl tyl_clone
            then ty
            else Tconstr (id, tyl_clone)
    
    let accu_mutable = ImmutableDictionary.CreateBuilder()
    let accu_immutable = tyenv.values_typeexpr_immutable.ToBuilder()
    for kv in tyenv.values_typeexpr_mutable do
        let ty = repr kv.Value.vi_type
        let ty' = ty_loop ty
        if LanguagePrimitives.PhysicalEquality ty ty' then
            accu_immutable.[kv.Key] <- kv.Value
        else
            accu_mutable.[kv.Key] <- { kv.Value with vi_type = ty' }

    { tyenv with
        values_typeexpr_immutable = accu_immutable.ToImmutable()
        values_typeexpr_mutable = accu_mutable.ToImmutable() }

let type_command_list warning_sink tyenv cmds =
    let mutable tyenv = tyenv_clone tyenv
    let tyenvs = ResizeArray()
    for cmd in cmds do
        match cmd.sc_desc with
        | SCtype dl ->
            let tyenv' = add_typedef tyenv cmd.sc_loc dl
            tyenvs.Add(tyenv)
            tyenv <- tyenv'
        | SChide name -> 
            let tyenv' = hide_type tyenv name cmd.sc_loc
            tyenvs.Add(tyenv)
            tyenv <- tyenv'
        | SCremove name ->
            if Option.isSome (try_get_value tyenv name) then
                let tyenv' =
                    { tyenv with
                        values_typeexpr_immutable = tyenv.values_typeexpr_immutable.Remove(name)
                        values_typeexpr_mutable = tyenv.values_typeexpr_mutable.Remove(name) }
                tyenvs.Add(tyenv')
                tyenv <- tyenv'
            else raise (Type_error ((Unbound_identifier name), cmd.sc_loc))
        | SCexn (name, tyl) ->
            if not (Char.IsUpper name.[0]) then
                raise (Type_error (Invalid_constructor_name, cmd.sc_loc))
            let tyl = List.map (type_expr tyenv None (Dictionary<string, type_expr>())) tyl
            let tyenv', _ = add_exn_constructor tyenv name tyl
            tyenvs.Add(tyenv)
            tyenv <- tyenv'
        | SCexpr e ->
            let ty = type_expression warning_sink tyenv e
            tyenvs.Add(tyenv)
            cmd.sc_desc <- SCCexpr (e, ty)
        | SCval _ | SCvar _ | SCfun _ ->
            let new_values = type_command warning_sink tyenv cmd
            let tyenv' = Types.add_values tyenv new_values
            tyenvs.Add(tyenv)
            cmd.sc_desc <- 
                match cmd.sc_desc with
                | SCval l -> SCCval (l, new_values)
                | SCvar l -> SCCvar (l, new_values)
                | SCfun l -> SCCfun (l, new_values)
                | _ -> dontcare()
            tyenv <- tyenv'
        | SClex lex_defs ->
            let ruless =
                try MalLex.Compile lex_defs
                with Failure msg -> raise (Type_error ((Invalid_lexer_definition msg), cmd.sc_loc))
            let accu = List<string * string list * HashSet<int> * DfaNode * expression array * location * Types.value_info>()
            tyenvs.Add(tyenv)
            for rules in ruless do
                // validate function names
                let names = Array.map (fun (name, _, _, _, _, _, _) -> name) rules
                Array.iter (fun name -> if is_constructor name then raise (Type_error (Invalid_identifier, cmd.sc_loc))) names
                all_differ cmd.sc_loc kind.Function_name kind.Function_definition names
                
                // create tyenv with dummy binding for function names
                let dummy_infos = Array.map (fun name -> (name, { vi_type = new_tvar 1; vi_access = access.Immutable; })) names
                let tyenv_with_dummy_fun_defs = add_values tyenv dummy_infos

                let new_values = List()
                for name, args, _, _, _, actions, loc in rules do
                    all_differ loc kind.Variable_name kind.Function_definition args
                    let arg_infos = (List.map (fun arg -> (arg, { vi_type = new_tvar 1; vi_access = access.Immutable; })) args) @ [("lexbuf", { vi_type = Tconstr (type_id.LEXBUF, []); vi_access = Immutable })]
                    let tyenv_with_arg_defs = add_values tyenv_with_dummy_fun_defs arg_infos
                    let ty_res = new_tvar 1
                    for action in actions do
                        let ty_action = Typechecker(warning_sink, tyenv_with_arg_defs, 1).Expression None action
                        unify_exp tyenv_with_arg_defs action ty_action ty_res
                    let ty_fun = List.foldBack2 (fun name ty1 ty2 -> Tarrow (name, ty1, ty2)) (args @ ["lexbuf"]) (List.map (fun (_, info : value_info) -> info.vi_type) arg_infos) ty_res
                    new_values.Add((name, ty_fun))
                let new_values = new_values.ToArray()
                Array.iter (fun (_, ty) -> generalize 0 ty) new_values
                for i = 0 to rules.Length - 1 do
                    let _, dummy_info = dummy_infos.[i]
                    let _, ty = new_values.[i]
                    let _, _, _, _, _, _, loc = rules.[i]
                    let ty_expected = dummy_info.vi_type
                    try unify tyenv ty ty_expected
                    with Unify -> raise (Type_error (Type_mismatch (tyenv, Expression, ty, ty_expected), loc))
                let new_values = Array.map (fun (name, ty) -> (name, { vi_type = ty; vi_access = Immutable })) new_values
                for i = 0 to rules.Length - 1 do
                    let name, args, alphabets, dfa, dfa_node_table, actions, loc = rules.[i]
                    let _, value_info = new_values.[i]
                    accu.Add(name, args, alphabets, dfa, actions, loc, value_info)
                    warning_sink ((Lexer_created (name, dfa_node_table.Length)), loc)
                tyenv <- Types.add_values tyenv new_values
            cmd.sc_desc <- SCClex (accu.ToArray())
        | _ -> dontcare()

    tyenvs.Add(tyenv)
    tyenvs.ToArray()
