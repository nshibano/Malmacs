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

// The value type is defined as regular class (not discriminated unions) to
// allow only some kind of them have overridden Finalize method.
// Doing so minimizes number of objects which goes to "finalizer queue" of CLR runtime.

type MalValueKind =
    | INT = 0
    | FLOAT = 1
    | STRING = 2
    | BLOCK = 3
    | ARRAY = 4
    | PARTIAL = 5
    | CLOSURE = 6
    | FUNC = 7
    | KFUNC = 8
    | COROUTINE = 9
    | VAR = 10
    | OBJ = 11

type MalValue (kind : MalValueKind) =
    member x.Kind = kind

and MalInt (i : int) =
    inherit MalValue(MalValueKind.INT)
    member x.Get = i

and MalFloat (x : float) =
    inherit MalValue(MalValueKind.FLOAT)
    member this.Get = x

and MalString (s : string, mm : MemoryManager) =
    inherit MalValue(MalValueKind.STRING)
    member x.Get = s
    override x.Finalize() =
        Interlocked.Add(&mm.counter, - sizeof_string s.Length) |> ignore

and MalBlock (tag : int, fields : MalValue array, mm : MemoryManager) =
    inherit MalValue(MalValueKind.BLOCK)
    member x.Tag = tag
    member x.Fields = fields
    override x.Finalize() =
        Interlocked.Add(&mm.counter, - sizeof_block fields.Length) |> ignore

and MalArray (count : int, storage: MalValue array, mm : MemoryManager) =
    inherit MalValue(MalValueKind.ARRAY)
    let mutable count = count
    let mutable storage = storage
    member ary.Count with get() = count and set n = count <- n
    member ary.Storage with get() = storage and set a = storage <- a
    member ary.MemoryManager = mm
    override x.Finalize() =
        Interlocked.Add(&mm.counter, - sizeof_array storage.Length) |> ignore

and MalFunc(arity : int, f : MemoryManager -> MalValue array -> MalValue) =
    inherit MalValue(MalValueKind.FUNC)
    member x.Arity = arity
    member x.F = f

and MalKFunc(arity : int, f : MemoryManager -> MalValue array -> MalValue array) =
    inherit MalValue(MalValueKind.KFUNC)
    member x.Arity = arity
    member x.F = f

and MalCoroutine(arity : int, starter : MemoryManager -> MalValue array -> IMalCoroutine) =
    inherit MalValue(MalValueKind.COROUTINE)
    member x.Arity = arity
    member x.Starter = starter

and MalClosure(arity : int, envSize : int, captures : MalValue array, code : code) =
    inherit MalValue(MalValueKind.CLOSURE)
    member x.Arity = arity
    member x.EnvSize = envSize
    member x.Captures = captures
    member x.Code = code

and MalPartial (arity : int, args : MalValue array) =
    inherit MalValue(MalValueKind.PARTIAL)
    member x.Arity = arity
    member x.Args = args

and MalVar (value : MalValue) =
    inherit MalValue(MalValueKind.VAR)
    let mutable content = value
    member var.Content with get() = content and set x = content <- x

and MalObj (obj : obj) =
    inherit MalValue(MalValueKind.OBJ)
    member x.Obj = obj

and IMalCoroutine =
    inherit IDisposable
    abstract member IsFinished : bool
    abstract member Run : milliseconds : int -> unit
    abstract member Result : MalValue

and code =
  // expressions
  | UEconst of MalValue
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

and MemoryManager =
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
        MemoryManager.counter = 0;
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
        MemoryManager.counter = 0
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

let checkFreeMemory (mm : MemoryManager) neededBytes =
    Volatile.Read(&mm.counter) + neededBytes < mm.bytes_trigger_gc ||
    (collect()
     Volatile.Read(&mm.counter) + neededBytes < mm.bytes_stop_exec)

let ofInt i = MalInt(i) :> MalValue

let toInt (v : MalValue) = (v :?> MalInt).Get

let ofChar (c : char) = ofInt (int c)

let toChar (v : MalValue) =
    let i = toInt v
    if int Char.MinValue <= i && i <= int Char.MaxValue then
        char i
    else
        dontcare()

let ofFloat x = MalFloat(x) :> MalValue

let toFloat (v : MalValue) = (v :?> MalFloat).Get

let zero = ofInt 0
let one = ofInt 1
let neg_one = ofInt (-1)

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
    match toInt v with
    | 0 -> false
    | 1 -> true
    | _ -> dontcare()

let to_string (v : MalValue) = (v :?> MalString).Get

let of_string (mm : MemoryManager) (s : string) =
    Interlocked.Add(&mm.counter, sizeof_string s.Length) |> ignore
    MalString(s, mm) :> MalValue

let to_obj (v : MalValue) = (v :?> MalObj).Obj

let of_obj (o : obj) = MalObj(o) :> MalValue

let block_create (mm : MemoryManager) tag (fields : MalValue array) =
    Interlocked.Add(&mm.counter, sizeof_block fields.Length) |> ignore
    MalBlock(tag, fields, mm) :> MalValue

let get_tag (v : MalValue) =
    match v.Kind with
    | MalValueKind.INT -> toInt v
    | MalValueKind.BLOCK -> (v :?> MalBlock).Tag
    | _ -> dontcare()

let value_array_empty : MalValue array = [||]

let get_fields (v : MalValue) =
    match v.Kind with
    | MalValueKind.INT -> value_array_empty
    | MalValueKind.BLOCK -> (v :?> MalBlock).Fields
    | _ -> dontcare()

let to_malarray (v : MalValue) = v :?> MalArray

exception MalException of MalValue
exception MalUncatchableException of string

let mal_failwith mm msg = raise (MalException (block_create mm tag_exn_Failure [| of_string mm msg |]))

let mal_Division_by_zero = ofInt tag_exn_Division_by_zero
let mal_raise_Division_by_zero() = raise (MalException mal_Division_by_zero)

let mal_Index_out_of_range = ofInt tag_exn_Index_out_of_range
let mal_raise_Index_out_of_range() = raise (MalException mal_Index_out_of_range)

let mal_Invalid_argument = ofInt tag_exn_Invalid_argument
let mal_raise_Invalid_argument() = raise (MalException mal_Invalid_argument)

let mal_Match_failure = ofInt tag_exn_Match_failure
let mal_raise_Match_failure() = raise (MalException mal_Match_failure)

let mal_raise_Insufficient_memory() = raise (MalUncatchableException "Insufficient memory")

let array_create (mm : MemoryManager) (needed_capacity : int) =
    let capacity =
        try find_next_capacity_exn mm.maximum_array_length needed_capacity
        with :? InvalidOperationException -> mal_raise_Insufficient_memory()
    let bytes = sizeof_array capacity
    if not (checkFreeMemory mm bytes) then mal_raise_Insufficient_memory()
    Interlocked.Add(&mm.counter, bytes) |> ignore
    MalArray(0, Array.zeroCreate<MalValue> capacity, mm) :> MalValue

let array_add (mm : MemoryManager) (ary : MalValue) (item : MalValue) =
    let ary = ary :?> MalArray
    let capacity = ary.Storage.Length //if isNull ary.Storage then 0 else storage.Length
    if capacity < ary.Count + 1 then
        let new_capacity =
            try find_next_capacity_exn mm.maximum_array_length (ary.Count + 1)
            with :? InvalidOperationException -> mal_raise_Insufficient_memory()
        let increased_bytes = value_array_increment * (new_capacity - capacity)
        if not (checkFreeMemory mm increased_bytes) then mal_raise_Insufficient_memory()
        let new_storage = Array.zeroCreate<MalValue> new_capacity
        if not (isNull ary.Storage) then Array.blit ary.Storage 0 new_storage 0 ary.Count
        ary.Storage <- new_storage
        Interlocked.Add(&ary.MemoryManager.counter, increased_bytes) |> ignore
    ary.Storage.[ary.Count] <- item
    ary.Count <- ary.Count + 1

let array_append (mm : MemoryManager) (a : MalValue) (b : MalValue) =
    let a = a :?> MalArray
    let b = b :?> MalArray
    let c_count = a.Count + b.Count
    let c = array_create mm c_count :?> MalArray
    Array.blit a.Storage 0 c.Storage 0 a.Count
    Array.blit b.Storage 0 c.Storage a.Count b.Count
    c.Count <- c_count
    c :> MalValue

let array_get (mm : MemoryManager) (v : MalValue) (i : int) =
    let ary = v :?> MalArray
    if 0 <= i && i < ary.Count then
        ary.Storage.[i]
    else raise (IndexOutOfRangeException())

let array_set (ary : MalValue) i (x : MalValue) =
    let ary = ary :?> MalArray
    if 0 <= i && i < ary.Count then
        ary.Storage.[i] <- x
    else raise (IndexOutOfRangeException())

let array_remove_at (mm : MemoryManager) (v : MalValue) i =
    let ary = v :?> MalArray
    if 0 <= i && i < ary.Count then
        for j = i + 1 to ary.Count - 1 do
            ary.Storage.[j - 1] <- ary.Storage.[j]
        ary.Storage.[ary.Count - 1] <- Unchecked.defaultof<MalValue>
        ary.Count <- ary.Count - 1
    else raise (IndexOutOfRangeException())

let array_clear (mm : MemoryManager) (v : MalValue) =
    let ary = v :?> MalArray
    for i = 0 to ary.Count - 1 do
        ary.Storage.[i] <- Unchecked.defaultof<MalValue>
    ary.Count <- 0

let array_copy (mm : MemoryManager) (orig : MalValue) =
    let orig = orig :?> MalArray
    let copy = array_create mm orig.Count :?> MalArray
    Array.blit orig.Storage 0 copy.Storage 0 orig.Count
    copy.Count <- orig.Count
    copy :> MalValue

let rec obj_of_value (cache : Dictionary<Type, HashSet<MalValue> -> MalValue -> obj>) (tyenv : tyenv) (touch : HashSet<MalValue>) (ty : Type) (value : MalValue) =
    if touch.Contains(value) then mal_failwith dummy_mm "cyclic value in interop"
    match cache.TryGetValue(ty) with
    | true, f -> f touch value
    | false, _ ->
        let f =
            if ty = typeof<unit> then
                (fun (touch : HashSet<MalValue>) (value : MalValue) -> box ())
            elif ty = typeof<bool> then
                (fun (touch : HashSet<MalValue>) (value : MalValue) -> box (to_bool value))
            elif ty = typeof<int32> then
                (fun (touch : HashSet<MalValue>) (value : MalValue) -> box (toInt value))
            elif ty = typeof<char> then
                (fun (touch : HashSet<MalValue>) (value : MalValue) -> box (toChar value))
            elif ty = typeof<float> then
                (fun (touch : HashSet<MalValue>) (value : MalValue) -> box (toFloat value))
            elif ty = typeof<string> then
                (fun (touch : HashSet<MalValue>) (value : MalValue) -> box (to_string value))
            elif tyenv.registered_abstract_types.ContainsKey(ty) then
                (fun (touch : HashSet<MalValue>) (value : MalValue) -> to_obj value)
            elif ty.IsArray then
                let ty_elem = ty.GetElementType()
                (fun (touch : HashSet<MalValue>) (value : MalValue) ->
                    let ary = value :?> MalArray
                    let array = System.Array.CreateInstance(ty_elem, ary.Count)
                    touch.Add(value) |> ignore
                    for i = 0 to ary.Count - 1 do
                        array.SetValue(obj_of_value cache tyenv touch ty_elem ary.Storage.[i], i)
                    touch.Remove(value) |> ignore
                    array :> obj)
            elif FSharpType.IsTuple ty then
                let constr = FSharpValue.PreComputeTupleConstructor(ty)
                let types = FSharpType.GetTupleElements(ty)
                (fun (touch : HashSet<MalValue>) (value : MalValue) ->
                    let fields = get_fields value
                    touch.Add(value) |> ignore
                    let objs = Array.map2 (fun ty field -> obj_of_value cache tyenv touch ty field) types fields
                    touch.Remove(value) |> ignore
                    constr objs)
            elif FSharpType.IsRecord ty then
                let constr = FSharpValue.PreComputeRecordConstructor(ty)
                let types = Array.map (fun (info : PropertyInfo) -> info.PropertyType) (FSharpType.GetRecordFields(ty))
                (fun (touch : HashSet<MalValue>) (value : MalValue) ->
                    let fields = get_fields value
                    touch.Add(value) |> ignore
                    let objs = Array.map2 (fun ty value -> obj_of_value cache tyenv touch ty value) types fields
                    touch.Remove(value) |> ignore
                    constr objs)
            elif FSharpType.IsUnion ty then
                let cases = FSharpType.GetUnionCases(ty)
                let constrs = Array.map (fun (case : UnionCaseInfo) -> FSharpValue.PreComputeUnionConstructor(case)) cases
                let case_field_types = Array.map (fun (case : UnionCaseInfo) -> Array.map (fun (prop : PropertyInfo) -> prop.PropertyType) (case.GetFields())) cases
                (fun (touch : HashSet<MalValue>) (value : MalValue) ->
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

let rec value_of_obj (cache : Dictionary<Type, MemoryManager -> obj -> MalValue>) (tyenv : tyenv) (ty : Type) (mm : MemoryManager) (obj : obj) =
    match cache.TryGetValue(ty) with
    | true, f -> f mm obj
    | false, _ ->
        let f =
            if ty = typeof<unit> then
                (fun (mm : MemoryManager) (obj : obj) -> unit)
            elif ty = typeof<bool> then
                (fun (mm : MemoryManager) (obj : obj) -> of_bool (obj :?> bool))
            elif ty = typeof<int32> then
                (fun (mm : MemoryManager) (obj : obj) -> ofInt (obj :?> int32))
            elif ty = typeof<char> then
                (fun (mm : MemoryManager) (obj : obj) -> ofChar (obj :?> char))
            elif ty = typeof<float> then
                (fun (mm : MemoryManager) (obj : obj) -> ofFloat (obj :?> float))
            elif ty = typeof<string> then
                (fun (mm : MemoryManager) (obj : obj) -> of_string mm (obj :?> string))
            elif tyenv.registered_abstract_types.ContainsKey(ty) then
                (fun (mm : MemoryManager) (obj : obj) -> of_obj obj)
            elif ty.IsArray then
                let ty_elem = ty.GetElementType()
                (fun (mm : MemoryManager) (obj : obj) ->
                    let ary = obj :?> System.Array
                    let len = ary.Length
                    let malary = array_create mm len
                    for i = 0 to len - 1 do
                        array_add mm malary (value_of_obj cache tyenv ty_elem mm (ary.GetValue(i)))
                    malary)
            elif FSharpType.IsTuple ty then
                let reader = FSharpValue.PreComputeTupleReader(ty)
                let types = FSharpType.GetTupleElements(ty)
                (fun (mm : MemoryManager) (obj : obj) -> 
                    let objs = reader obj
                    let values = Array.map2 (fun ty obj -> value_of_obj cache tyenv ty mm obj) types objs
                    block_create mm 0 values)
            elif FSharpType.IsRecord ty then
                let reader = FSharpValue.PreComputeRecordReader(ty)
                let types = Array.map (fun (info : PropertyInfo) -> info.PropertyType) (FSharpType.GetRecordFields(ty))
                (fun (mm : MemoryManager) (obj : obj) -> 
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
                            (fun (mm : MemoryManager) (obj : obj) ->
                                ofInt i)
                        else
                            let field_types = Array.map (fun (field : PropertyInfo) -> field.PropertyType) fields
                            let case_reader = FSharpValue.PreComputeUnionReader(case)
                            (fun (mm : MemoryManager) (obj : obj) ->
                                let field_objs = case_reader obj
                                let field_vals = Array.map2 (fun ty obj -> value_of_obj cache tyenv ty mm obj) field_types field_objs
                                block_create mm i field_vals)) cases
                (fun (mm : MemoryManager) (obj : obj) ->
                    let tag = tag_reader obj
                    fs.[tag] mm obj)
            else
                raise (NotImplementedException())

        cache.[ty] <- f
        f mm obj

let touch_create() = HashSet<MalValue>(Misc.PhysicalEqualityComparer)

let wrap_fsharp_func (tyenv : tyenv) (obj_of_value_cache : Dictionary<Type, HashSet<MalValue> -> MalValue -> obj>) (value_of_obj_cache : Dictionary<Type, MemoryManager -> obj -> MalValue>) (ty : Type) (func : obj) =
    let rec flatten ty =
        if FSharpType.IsFunction ty then
            let t1, t2 = FSharpType.GetFunctionElements ty
            t1 :: flatten t2
        else
            [ty]
    let tyl = Array.ofList (flatten ty)
    if not (3 <= tyl.Length && tyl.Length <= 6 && tyl.[0] = typeof<MemoryManager>) then dontcare()
    let arity = tyl.Length - 2
    let invokefast =
        let methods = typedefof<FSharpFunc<_, _>>.MakeGenericType(tyl.[0], tyl.[1]).GetMethods()
        let invokefast_gen = Array.find (fun (mi : MethodInfo) -> mi.Name = "InvokeFast" && mi.GetParameters().Length = 2 + arity) methods
        invokefast_gen.MakeGenericMethod(Array.sub tyl 2 arity)
    let func (mm : MemoryManager) (argv : MalValue array) =
        let touch = touch_create()
        let arg_objs = Array.init arity (fun i -> obj_of_value obj_of_value_cache tyenv touch tyl.[i+1] argv.[i])
        let result_obj =
            try invokefast.Invoke(null, Array.append [| func; mm; |] arg_objs)
            with :? System.Reflection.TargetInvocationException as exn -> raise exn.InnerException
        value_of_obj value_of_obj_cache tyenv tyl.[tyl.Length - 1] mm result_obj
    MalFunc(arity, func) :> MalValue
