module FsMiniMAL.Value

open System
open System.Collections.Generic
open System.Threading
open System.Reflection
open FSharp.Reflection

open Types

let sizeof_int, sizeof_float, block_overhead, array_overhead, value_array_increment, string_overhead, string_increment =
    if IntPtr.Size = 8
    then 32, 40, 64, 96, 8, 70, 2
    else 20, 24, 36, 48, 4, 34, 2

let sizeof_block len = block_overhead + value_array_increment * len
let sizeof_array cap = array_overhead + value_array_increment * cap
let sizeof_string len = string_overhead + string_increment * len

type value =
    | Vint of int * memory_manager
    | Vfloat of float * memory_manager
    | Vblock of int * value array * memory_manager
    | Varray of malarray
    | Vstring of string * memory_manager
    | Vfunc of arity : int * func : (memory_manager -> value array -> value)
    | Vkfunc of arity : int * kfunc : (memory_manager -> value array -> value array)
    | Vcoroutine of arity : int * starter : (memory_manager -> value array -> IMalCoroutine)
    | Vclosure of arity : int * env_size : int * captures : value array * code : code
    | Vpartial of arity : int * args : value array
    | Vvar of value ref
    | Vobj of obj
    
    override this.Finalize() =
        match this with
        | Vint (_, mm) -> Interlocked.Add(&mm.counter, - sizeof_int) |> ignore
        | Vfloat (_, mm) -> Interlocked.Add(&mm.counter, - sizeof_float) |> ignore
        | Vblock (_, fields, mm) -> Interlocked.Add(&mm.counter, - sizeof_block fields.Length) |> ignore
        | Varray ary -> Interlocked.Add(&ary.memory_manager.counter, - sizeof_array ary.storage.Length) |> ignore
        | Vstring (s, mm) -> Interlocked.Add(&mm.counter, - sizeof_string s.Length) |> ignore
        | _ -> ()

and malarray =
    { mutable count : int
      mutable storage : value array
      memory_manager : memory_manager }

and IMalCoroutine =
    inherit IDisposable
    abstract member IsFinished : bool
    abstract member Run : milliseconds : int -> unit
    abstract member Result : value

and code =
  // expressions
  | UEconst of value
  | UEgetenv of int
  | UEgetenvvar of int
  | UEsetenvvar of int * code
  | UEgetcap of int
  | UEgetcapvar of int
  | UEsetcapvar of int * code
  | UEblock of tag : int * fields : code array
  | UEblockwith of orig : code * field_indexes : int array * field_values : code array // { orig with field = expr; ... }
  | UEarray of code array
  | UEapply of code array * Syntax.location
  | UEtailcall of code array
  | UEfn of env_size : int * arity : int * is_capture_flags : bool array * ofss : int array * code
  | UEbegin of code array
  | UEcase of code * (pattern * code option * code) array // (test_expr, [| (pat, when_clause_expr, result_expr); ... |])
  | UEtry of code * (pattern * code) array
  | UEif of code * code * code
  | UEfor of ofs : int * first : code * dirflag * last : code * body : code
  | UEwhile of code * code
  
  // dummy code
  | UEcoroutine

  // commands
  | UCval of (pattern * code) array
  | UCvar of (int * code) array
  | UCfun of (int * code) array

  // toplevel commands
  | UTCexpr of code * tyenv * alloc * type_expr
  | UTCvalvarfun of code * tyenv * alloc * int array * (string * value_info) list
  | UTCtype of string list * tyenv
  | UTChide of string * tyenv * alloc
  | UTCremove of string * int * tyenv * alloc
  | UTCexn of string * tyenv * alloc
  | UTClex of (int * int * HashSet<int> * Syntax.DfaNode * code array) array * tyenv * alloc * int array * (string * value_info) list // arity, offset, alphabets, dfa, actions
  | UTCupd of tyenv * alloc

and pattern =
  | UPid of int
  | UPas of pattern * int
  | UPint of int
  | UPchar of char
  | UPfloat of float
  | UPstring of string
  | UPblock of int * pattern array
  | UParray of pattern array
  | UPor of pattern * pattern
  | UPany

and memory_manager =
    {       
      /// Total bytes used by mal values this interpreter owns.
      /// This field is increased when new mal value is created, and decreased when mal value is freed by CLR garbage collector. 
      mutable counter : int

      /// When counter value exceedes this limit, GC will be called.
      bytes_trigger_gc : int

      /// After GC, if the counter still exceeds this limit, interpreter is stopped.
      bytes_stop_exec : int
      
      maximum_array_length : int
      maximum_stack_depth : int
      maximum_compare_stack_depth : int
      coroutine_initial_ticks : int
    }

let memory_manager_create_default() =
    {
        memory_manager.counter = 0;
        bytes_trigger_gc = 256 <<< 20 // 256 MiB
        bytes_stop_exec = 128 <<< 20 // 128 MiB

        /// A practical limit for length of arrays. 128MiB for value array on 64 bit machine.
        maximum_array_length = 1 <<< 24 // 16777216

        maximum_stack_depth = 1 <<< 20 // 1048576
        maximum_compare_stack_depth = 1 <<< 20 // 1048576
        coroutine_initial_ticks = 1
    }

let dummy_mm =
    { 
        memory_manager.counter = 0
        bytes_trigger_gc = Int32.MaxValue
        bytes_stop_exec = Int32.MaxValue
        maximum_array_length = Int32.MaxValue
        maximum_stack_depth = Int32.MaxValue
        maximum_compare_stack_depth = Int32.MaxValue
        coroutine_initial_ticks = Int32.MaxValue
    }

let collect() =
    GC.Collect()
    GC.WaitForPendingFinalizers()
    GC.Collect()

let check_free_memory (mm : memory_manager) needed_bytes =
    Volatile.Read(&mm.counter) + needed_bytes < mm.bytes_trigger_gc ||
    (collect()
     Volatile.Read(&mm.counter) + needed_bytes < mm.bytes_stop_exec)

let of_int (mm : memory_manager) i =
    Interlocked.Add(&mm.counter, sizeof_int) |> ignore
    Vint (i, mm)

let to_int v = match v with Vint (i, _) -> i | _ -> dontcare()

let of_char (mm : memory_manager) (c : char) = of_int mm (int c)

let to_char (v : value) =
    let i = to_int v
    if int Char.MinValue <= i && i <= int Char.MaxValue then
        char i
    else
        dontcare()

let of_float (mm : memory_manager) x =
    Interlocked.Add(&mm.counter, sizeof_float) |> ignore
    Vfloat (x, mm)

let to_float v = match v with Vfloat (x, _) -> x | _ -> dontcare()

let zero = of_int dummy_mm 0
let one = of_int dummy_mm 1
let neg_one = of_int dummy_mm (-1)

let of_compare n =
    match n with
    | 1 -> one
    | 0 -> zero
    | -1 -> neg_one
    | _ -> dontcare()

let unit = zero
let ``false`` = zero
let ``true`` = one

let of_bool b = if b then ``true`` else ``false``

let to_bool v =
    match v with
    | Vint (0, _) -> false
    | Vint (1, _) -> true
    | _ -> dontcare()

let to_string v =
    match v with
    | Vstring (s, _) -> s
    | _ -> dontcare()

let of_string (mm : memory_manager) (s : string) =
    Interlocked.Add(&mm.counter, sizeof_string s.Length) |> ignore
    Vstring (s, mm)

let to_obj v =
    match v with
    | Vobj o -> o
    | _ -> dontcare()

let of_obj o = Vobj o

let block_create (mm : memory_manager) tag (fields : value array) =
    Interlocked.Add(&mm.counter, sizeof_block fields.Length) |> ignore
    Vblock (tag, fields, mm)

let get_tag (v : value) =
    match v with
    | Vint (i, _) -> i
    | Vblock (tag, _, _) -> tag
    | _ -> dontcare()

let value_array_empty : value array = [||]

let get_fields (v : value) =
    match v with
    | Vint _ -> value_array_empty
    | Vblock (_, fields, _) -> fields
    | _ -> dontcare()

let to_malarray (v : value) =
    match v with
    | Varray malary -> malary
    | _ -> dontcare()

exception MalException of value
exception MalUncatchableException of string

let mal_failwith mm msg = raise (MalException (block_create mm tag_exn_Failure [| of_string mm msg |]))

let mal_Division_by_zero = of_int dummy_mm tag_exn_Division_by_zero
let mal_raise_Division_by_zero () = raise (MalException mal_Division_by_zero)

let mal_Index_out_of_range = of_int dummy_mm tag_exn_Index_out_of_range
let mal_raise_Index_out_of_range () = raise (MalException mal_Index_out_of_range)

let mal_Invalid_argument = of_int dummy_mm tag_exn_Invalid_argument
let mal_raise_Invalid_argument () = raise (MalException mal_Invalid_argument)

let mal_Match_failure = of_int dummy_mm tag_exn_Match_failure
let mal_raise_Match_failure () = raise (MalException mal_Match_failure)

let mal_raise_Insufficient_memory() = raise (MalUncatchableException "Insufficient memory")

let array_create (mm : memory_manager) (needed_capacity : int) =
    let capacity =
        try find_next_capacity_exn mm.maximum_array_length needed_capacity
        with :? InvalidOperationException -> mal_raise_Insufficient_memory()
    let bytes = sizeof_array capacity
    if not (check_free_memory mm bytes) then mal_raise_Insufficient_memory()
    Interlocked.Add(&mm.counter, bytes) |> ignore
    Varray { count = 0; storage = Array.zeroCreate<value> capacity; memory_manager = mm }

let array_add (mm : memory_manager) (ary : value) (item : value) =
    match ary with  
    | Varray ({ count = count; storage = storage } as ary) ->
        let capacity = if isNull storage then 0 else storage.Length
        if capacity < count + 1 then
            let new_capacity =
                try find_next_capacity_exn mm.maximum_array_length (count + 1)
                with :? InvalidOperationException -> mal_raise_Insufficient_memory()
            let increased_bytes = value_array_increment * (new_capacity - capacity)
            if not (check_free_memory mm increased_bytes) then mal_raise_Insufficient_memory()
            let new_storage = Array.zeroCreate<value> new_capacity
            if not (isNull storage) then Array.blit storage 0 new_storage 0 count
            ary.storage <- new_storage
            Interlocked.Add(&ary.memory_manager.counter, increased_bytes) |> ignore
        ary.storage.[ary.count] <- item
        ary.count <- ary.count + 1
    | _ -> dontcare()

let array_append (mm : memory_manager) (a : value) (b : value) =
    match a, b with
    | Varray { count = a_count; storage = a_storage }, Varray { count = b_count; storage = b_storage } ->
        let c_count = a_count + b_count
        let c = array_create mm c_count
        match c with
        | Varray ({ storage = c_storage } as c_ary) ->
            if a_count <> 0 then Array.blit a_storage 0 c_storage 0 a_count
            if b_count <> 0 then Array.blit b_storage 0 c_storage a_count b_count
            c_ary.count <- c_count
            c
        | _ -> dontcare()
    | _ -> dontcare()

let array_get (mm : memory_manager) (v : value) (i : int) =
    match v with
    | Varray ary ->
        if 0 <= i && i < ary.count then
            ary.storage.[i]
        else raise (IndexOutOfRangeException())
    | _ -> dontcare()

let array_set (ary : value) i (x : value) =
    match ary with
    | Varray ary ->
        if 0 <= i && i < ary.count then
            ary.storage.[i] <- x
        else raise (IndexOutOfRangeException())
    | _ -> dontcare()

let array_remove_at (mm : memory_manager) (v : value) i =
    match v with
    | Varray ary ->
        if 0 <= i && i < ary.count then
            for j = i + 1 to ary.count - 1 do
                ary.storage.[j - 1] <- ary.storage.[j]
            ary.storage.[ary.count - 1] <- Unchecked.defaultof<value>
            ary.count <- ary.count - 1
        else raise (IndexOutOfRangeException())
    | _ -> dontcare()

let array_clear (mm : memory_manager) (v : value) =
    match v with
    | Varray ary ->
        for i = 0 to ary.count - 1 do
            ary.storage.[i] <- Unchecked.defaultof<value>
        ary.count <- 0
    | _ -> dontcare()

let array_copy (mm : memory_manager) (orig : value) =
    match orig with
    | Varray { count = count; storage = storage } ->
        let copy = array_create mm count
        match copy with
        | Varray ({ storage = copy_storage } as copy_ary) ->
            if count <> 0 then Array.blit storage 0 copy_storage 0 count;
            copy_ary.count <- count
            copy
        | _ -> dontcare()
    | _ -> dontcare()

let rec obj_of_value (cache : Dictionary<Type, HashSet<value> -> value -> obj>) (tyenv : tyenv) (touch : HashSet<value>) (ty : Type) (value : value) =
    if touch.Contains(value) then mal_failwith dummy_mm "cyclic value in interop"
    match cache.TryGetValue(ty) with
    | true, f -> f touch value
    | false, _ ->
        let f =
            if ty = typeof<unit> then
                (fun (touch : HashSet<value>) (value : value) -> box ())
            elif ty = typeof<bool> then
                (fun (touch : HashSet<value>) (value : value) -> box (to_bool value))
            elif ty = typeof<int32> then
                (fun (touch : HashSet<value>) (value : value) -> box (to_int value))
            elif ty = typeof<char> then
                (fun (touch : HashSet<value>) (value : value) -> box (to_char value))
            elif ty = typeof<float> then
                (fun (touch : HashSet<value>) (value : value) -> box (to_float value))
            elif ty = typeof<string> then
                (fun (touch : HashSet<value>) (value : value) -> box (to_string value))
            elif tyenv.registered_abstract_types.ContainsKey(ty) then
                (fun (touch : HashSet<value>) (value : value) -> to_obj value)
            elif ty.IsArray then
                let ty_elem = ty.GetElementType()
                (fun (touch : HashSet<value>) (value : value) ->
                    match value with
                    | Varray malarray ->
                        let array = System.Array.CreateInstance(ty_elem, malarray.count)
                        touch.Add(value) |> ignore
                        for i = 0 to malarray.count - 1 do
                            array.SetValue(obj_of_value cache tyenv touch ty_elem malarray.storage.[i], i)
                        touch.Remove(value) |> ignore
                        array :> obj
                    | _ -> dontcare())
            elif FSharpType.IsTuple ty then
                let constr = FSharpValue.PreComputeTupleConstructor(ty)
                let types = FSharpType.GetTupleElements(ty)
                (fun (touch : HashSet<value>) (value : value) ->
                    let fields = get_fields value
                    touch.Add(value) |> ignore
                    let objs = Array.map2 (fun ty field -> obj_of_value cache tyenv touch ty field) types fields
                    touch.Remove(value) |> ignore
                    constr objs)
            elif FSharpType.IsRecord ty then
                let constr = FSharpValue.PreComputeRecordConstructor(ty)
                let types = Array.map (fun (info : PropertyInfo) -> info.PropertyType) (FSharpType.GetRecordFields(ty))
                (fun (touch : HashSet<value>) (value : value) ->
                    let fields = get_fields value
                    touch.Add(value) |> ignore
                    let objs = Array.map2 (fun ty value -> obj_of_value cache tyenv touch ty value) types fields
                    touch.Remove(value) |> ignore
                    constr objs)
            elif FSharpType.IsUnion ty then
                let cases = FSharpType.GetUnionCases(ty)
                let constrs = Array.map (fun (case : UnionCaseInfo) -> FSharpValue.PreComputeUnionConstructor(case)) cases
                let case_field_types = Array.map (fun (case : UnionCaseInfo) -> Array.map (fun (prop : PropertyInfo) -> prop.PropertyType) (case.GetFields())) cases
                (fun (touch : HashSet<value>) (value : value) ->
                    let tag = get_tag value
                    let types = case_field_types.[tag]
                    let fields = get_fields value
                    touch.Add(value) |> ignore
                    let objs = Array.map2 (fun ty value -> obj_of_value cache tyenv touch ty value) types fields
                    touch.Remove(value) |> ignore
                    constrs.[tag] objs)
            else
                raise (NotImplementedException())
        cache.[ty] <- f
        f touch value

let rec value_of_obj (cache : Dictionary<Type, memory_manager -> obj -> value>) (tyenv : tyenv) (ty : Type) (mm : memory_manager) (obj : obj) =
    match cache.TryGetValue(ty) with
    | true, f -> f mm obj
    | false, _ ->
        let f =
            if ty = typeof<unit> then
                (fun (mm : memory_manager) (obj : obj) -> unit)
            elif ty = typeof<bool> then
                (fun (mm : memory_manager) (obj : obj) -> of_bool (obj :?> bool))
            elif ty = typeof<int32> then
                (fun (mm : memory_manager) (obj : obj) -> of_int mm (obj :?> int32))
            elif ty = typeof<char> then
                (fun (mm : memory_manager) (obj : obj) -> of_char mm (obj :?> char))
            elif ty = typeof<float> then
                (fun (mm : memory_manager) (obj : obj) -> of_float mm (obj :?> float))
            elif ty = typeof<string> then
                (fun (mm : memory_manager) (obj : obj) -> of_string mm (obj :?> string))
            elif tyenv.registered_abstract_types.ContainsKey(ty) then
                (fun (mm : memory_manager) (obj : obj) -> of_obj obj)
            elif ty.IsArray then
                let ty_elem = ty.GetElementType()
                (fun (mm : memory_manager) (obj : obj) ->
                    let ary = obj :?> System.Array
                    let len = ary.Length
                    let malary = array_create mm len
                    for i = 0 to len - 1 do
                        array_add mm malary (value_of_obj cache tyenv ty_elem mm (ary.GetValue(i)))
                    malary)
            elif FSharpType.IsTuple ty then
                let reader = FSharpValue.PreComputeTupleReader(ty)
                let types = FSharpType.GetTupleElements(ty)
                (fun (mm : memory_manager) (obj : obj) -> 
                    let objs = reader obj
                    let values = Array.map2 (fun ty obj -> value_of_obj cache tyenv ty mm obj) types objs
                    block_create mm 0 values)
            elif FSharpType.IsRecord ty then
                let reader = FSharpValue.PreComputeRecordReader(ty)
                let types = Array.map (fun (info : PropertyInfo) -> info.PropertyType) (FSharpType.GetRecordFields(ty))
                (fun (mm : memory_manager) (obj : obj) -> 
                    let objs = reader obj
                    let values = Array.map2 (fun ty obj -> value_of_obj cache tyenv ty mm obj) types objs
                    block_create mm 0 values)
            elif FSharpType.IsUnion ty then
                let tag_reader = FSharpValue.PreComputeUnionTagReader(ty)
                let cases = FSharpType.GetUnionCases(ty)
                let fs =
                    Array.mapi (fun i (case : UnionCaseInfo) ->
                        if i <> case.Tag then dontcare()
                        let fields = case.GetFields()
                        if fields.Length = 0 then
                            (fun (mm : memory_manager) (obj : obj) ->
                                of_int mm i)
                        else
                            let field_types = Array.map (fun (field : PropertyInfo) -> field.PropertyType) fields
                            let case_reader = FSharpValue.PreComputeUnionReader(case)
                            (fun (mm : memory_manager) (obj : obj) ->
                                let field_objs = case_reader obj
                                let field_vals = Array.map2 (fun ty obj -> value_of_obj cache tyenv ty mm obj) field_types field_objs
                                block_create mm i field_vals)) cases
                (fun (mm : memory_manager) (obj : obj) ->
                    let tag = tag_reader obj
                    fs.[tag] mm obj)
            else
                raise (NotImplementedException())

        cache.[ty] <- f
        f mm obj

let touch_create() = HashSet<value>(Misc.PhysicalEqualityComparer)

let wrap_fsharp_func (tyenv : tyenv) (obj_of_value_cache : Dictionary<Type, HashSet<value> -> value -> obj>) (value_of_obj_cache : Dictionary<Type, memory_manager -> obj -> value>) (ty : Type) (func : obj) =
    let rec flatten ty =
        if FSharpType.IsFunction ty then
            let t1, t2 = FSharpType.GetFunctionElements ty
            t1 :: flatten t2
        else
            [ty]
    let tyl = Array.ofList (flatten ty)
    if not (3 <= tyl.Length && tyl.Length <= 6 && tyl.[0] = typeof<memory_manager>) then dontcare()
    let arity = tyl.Length - 2
    let invokefast =
        let methods = typedefof<FSharpFunc<_, _>>.MakeGenericType(tyl.[0], tyl.[1]).GetMethods()
        let invokefast_gen = Array.find (fun (mi : MethodInfo) -> mi.Name = "InvokeFast" && mi.GetParameters().Length = 2 + arity) methods
        invokefast_gen.MakeGenericMethod(Array.sub tyl 2 arity)
    let func (mm : memory_manager) (argv : value array) =
        let touch = touch_create()
        let arg_objs = Array.init arity (fun i -> obj_of_value obj_of_value_cache tyenv touch tyl.[i+1] argv.[i])
        let result_obj =
            try invokefast.Invoke(null, Array.append [| func; mm; |] arg_objs)
            with :? System.Reflection.TargetInvocationException as exn -> raise exn.InnerException
        value_of_obj value_of_obj_cache tyenv tyl.[tyl.Length - 1] mm result_obj
    Vfunc (arity, func)
