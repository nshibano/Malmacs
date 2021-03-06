﻿module FsMiniMAL.Top

open System
open System.Text
open System.Diagnostics
open System.Collections.Generic

open Types
open Value
open Typechk

let tyenv_std, alloc_std, genv_std =
    let mutable tyenv = Types.tyenv_basic
    let mutable genv = [||]
    let mutable alloc = alloc.Create()

    let add name ty value =
        tyenv <- Types.add_value tyenv name { vi_access = access.Immutable; vi_type = ty }
        let ofs = alloc.Add(name, access.Immutable)
        genv <- array_ensure_capacity_exn Int32.MaxValue (ofs + 1) genv
        genv.[ofs] <- value

    let add_func name ty arity func = add name ty (MalFunc(arity, func) :> MalValue)
    let add_i name i = add name ty_int (ofInt i)    
    let add_ii name (f : int -> int) = add_func name ty_ii 1 (fun mm argv -> ofInt (f (toInt argv.[0])))
    let add_iii name (f : int -> int -> int) = add_func name ty_iii 2 (fun mm argv -> ofInt (f (toInt argv.[0]) (toInt argv.[1])))
    let add_f name x = add name ty_float (ofFloat x)
    let add_ff name (f : float -> float) = add_func name ty_ff 1 (fun mm argv -> ofFloat (f (toFloat argv.[0])))
    let add_fff name (f : float -> float -> float) = add_func name ty_fff 2 (fun mm argv -> ofFloat (f (toFloat argv.[0]) (toFloat argv.[1])))
    let add_if name (f : int -> float) = add_func name ty_if 1 (fun mm argv -> ofFloat (f (toInt argv.[0])))
    let add_fi name (f : float -> int) = add_func name ty_fi 1 (fun mm argv -> ofInt (f (toFloat argv.[0])))
    let add_vvb name (f : MalValue -> MalValue -> bool) = add_func name ty_vvb 2 (fun mm argv -> ofBool (f argv.[0] argv.[1]))
    let add_uu name (f : MemoryManager -> unit) = add_func name ty_uu 1 (fun mm argv -> f mm; unit)
    
    add "kprintf" (arrow2 (arrow ty_string ty_b) (Tconstr (type_id.FORMAT, [ty_a; ty_b])) ty_a)
        (MalKFunc (2, (fun mm frame ->
            let k = frame.[1]
            let cmds = (frame.[2] :?> MalObj).Obj :?> PrintfFormat.PrintfCommand list
            let cmds_arity = MalPrintf.arity_of_cmds cmds
            let arity_remain = cmds_arity - (frame.Length - 3)
            if arity_remain > 0 then
                [| MalPartial (arity_remain, frame) |]
            else
                let s = MalPrintf.exec_cmds cmds (Array.sub frame 3 cmds_arity)
                let j = 3 + cmds_arity
                Array.append [| k; ofString mm s |] (Array.sub frame j (frame.Length - j)))))
    add "compare" ty_vvi (MalCoroutine (2, fun mm argv -> new MalCompare.MalCompare(mm, MalCompare.Mode.Compare, argv) :> IMalCoroutine))
    add "=" ty_vvb (MalCoroutine (2, fun mm argv -> new MalCompare.MalCompare(mm, MalCompare.Mode.Equal, argv) :> IMalCoroutine))
    add "<>" ty_vvb (MalCoroutine (2, fun mm argv -> new MalCompare.MalCompare(mm, MalCompare.Mode.Not_equal, argv) :> IMalCoroutine))
    add "<" ty_vvb (MalCoroutine (2, fun mm argv -> new MalCompare.MalCompare(mm, MalCompare.Mode.Less_than, argv) :> IMalCoroutine))
    add ">" ty_vvb (MalCoroutine (2, fun mm argv -> new MalCompare.MalCompare(mm, MalCompare.Mode.Greater_than, argv) :> IMalCoroutine))
    add "<=" ty_vvb (MalCoroutine (2, fun mm argv -> new MalCompare.MalCompare(mm, MalCompare.Mode.Less_equal, argv) :> IMalCoroutine))
    add ">=" ty_vvb (MalCoroutine (2, fun mm argv -> new MalCompare.MalCompare(mm, MalCompare.Mode.Greater_equal, argv) :> IMalCoroutine))
    add_func "not" ty_bb 1 (fun mm argv -> ofBool (not (toBool argv.[0])))    
    add_func "&&" ty_bbb 2 (fun mm argv -> ofBool (toBool argv.[0] && toBool argv.[1]))
    add_func "||" ty_bbb 2 (fun mm argv -> ofBool (toBool argv.[0] || toBool argv.[1]))
    add_func "." ty_unit 2 (fun mm argv -> (getFields argv.[0]).[toInt argv.[1]])
    add_func ".<-" ty_unit 3 (fun mm argv -> (getFields argv.[0]).[toInt argv.[1]] <- argv.[2]; Value.unit)
    add_i "intMin" System.Int32.MinValue
    add_i "intMax" System.Int32.MaxValue
    add_ii "~-" ( ~- )
    add_ii "~~~" ( ~~~ )
    add_iii "+" ( + )
    add_iii "-" ( - )
    add_iii "*" ( * )
    add_iii "/" (fun a b -> if b <> 0 then a / b else mal_raise_Division_by_zero())
    add_iii "%" (fun a b -> if b <> 0 then a % b else mal_raise_Division_by_zero())
    add_iii "|||" ( ||| )
    add_iii "^^^" ( ^^^ )
    add_iii "&&&" ( &&& )
    add_iii ">>>" ( >>> )
    add_iii "<<<" ( <<< )
    add_f "infinity" infinity
    add_f "nan" nan
    add_ff "~-." ( ~- )
    add_ff "exp" exp
    add_ff "log" log
    add_ff "sqrt" sqrt
    add_ff "sin" sin
    add_ff "cos" cos
    add_ff "tan" tan
    add_ff "asin" asin
    add_ff "acos" acos
    add_ff "atan" atan
    add_ff "abs" abs
    add_fff "+." ( + )
    add_fff "-." ( - )
    add_fff "*." ( * )
    add_fff "/." ( / )
    add_fff "**" ( ** )
    add_if "float" (fun n -> float n)
    add_fi "round" (fun x -> int (round x))
    add_vvb "==" LanguagePrimitives.PhysicalEquality
    add_vvb "!=" (fun a b -> not (LanguagePrimitives.PhysicalEquality a b))

    add_func "arrayCreate" (arrow2 ty_int ty_a (ty_array ty_a)) 2
        (fun mm argv ->
            let n = toInt argv.[0]
            if n < 0 then mal_failwith mm "array_create: negative length"
            let x = argv.[1]
            let v = array_create mm n
            let ary = v :?> MalArray
            for i = 0 to n - 1 do
                ary.Storage.[i] <- x
            ary.Count <- n
            v)

    add_func "^" (arrow2 (ty_array ty_a) (ty_array ty_a) (ty_array ty_a)) 2 (fun mm argv -> array_append mm argv.[0] argv.[1])

    let string_append_func (mm : MemoryManager) (argv : MalValue array) =
        ofString mm (toString argv.[0] + toString argv.[1])
    add_func "stringAppend" ty_sss 2 string_append_func
    add_func "^^" ty_sss 2 string_append_func

    let array_get_func (mm : MemoryManager) (argv : MalValue array) =
        match argv.[0].Kind with
        | MalValueKind.STRING ->
            let s = (argv.[0] :?> MalString).Get
            let i = toInt argv.[1]
            if 0 <= i && i < s.Length then
                ofChar s.[i]
            else mal_raise_Index_out_of_range()
        | MalValueKind.ARRAY ->
            try array_get mm (argv.[0]) (toInt argv.[1])
            with :? IndexOutOfRangeException -> mal_raise_Index_out_of_range()
        | _ -> dontcare()
    let ty_array_get = arrow2 (ty_array ty_a) ty_int ty_a
    add_func ".[]" ty_array_get 2 array_get_func
    add_func "arrayGet" ty_array_get 2 array_get_func

    let array_set_func (g : MemoryManager) (argv : MalValue array) =
        try
            array_set argv.[0] (toInt argv.[1]) argv.[2]
            Value.unit
        with :? IndexOutOfRangeException -> mal_raise_Index_out_of_range()
    let ty_array_set = arrow3 (ty_array ty_a) ty_int ty_a ty_unit
    add_func ".[]<-" ty_array_set 3 array_set_func
    add_func "arraySet" ty_array_set 3 array_set_func

    add_func "arrayLength" (arrow (ty_array ty_a) ty_int) 1 (fun mm argv -> ofInt (argv.[0] :?> MalArray).Count)
    add_func "arrayCopy" (arrow (ty_array ty_a) (ty_array ty_a)) 1 (fun mm argv -> array_copy mm argv.[0])

    add_func "arraySub" (arrow3 (ty_array ty_a) ty_int ty_int (ty_array ty_a)) 3
        (fun mm argv ->
            let ary = argv.[0] :?> MalArray
            let start = toInt argv.[1]
            let count = toInt argv.[2]
            if not (0 <= count && 0 <= start && start + count <= ary.Count) then mal_raise_Index_out_of_range()
            let sub = array_create mm count
            let subary = sub :?> MalArray
            Array.blit ary.Storage start subary.Storage 0 count
            subary.Count <- count
            sub)

    add_func "arrayFill" (arrow4 (ty_array ty_a) ty_int ty_int ty_a ty_unit) 4
        (fun mm argv ->
            let ary = to_malarray argv.[0]
            let start = toInt argv.[1]
            let count = toInt argv.[2]
            let item = argv.[3]
            if not (0 <= count && 0 <= start && start + count <= ary.Count) then mal_raise_Index_out_of_range()
            Array.fill ary.Storage start count item
            unit)
        
    add_func "arrayBlit" (arrow5 (ty_array ty_a) ty_int (ty_array ty_a) ty_int ty_int ty_unit) 5
        (fun mm argv ->
            let src = to_malarray argv.[0]
            let i = toInt argv.[1]
            let dst = to_malarray argv.[2]
            let j = toInt argv.[3]
            let count = toInt argv.[4]
            if not (0 <= i && i + count <= src.Count && 0 <= j && j + count <= dst.Count) then mal_raise_Index_out_of_range()
            for k = 0 to count - 1 do
                dst.Storage.[j+k] <- src.Storage.[i+k]
            unit)

    add_func "stringLength" (arrow ty_string ty_int) 1 (fun mm argv -> ofInt (toString argv.[0]).Length)
    add_func "stringOfChar" (arrow ty_char ty_string) 1 (fun mm argv -> ofString mm (String(char (toInt argv.[0]), 1)))

    add_func "stringOfCharArray" (arrow (ty_array ty_char) ty_string) 1
        (fun mm argv ->
            let ary = to_malarray argv.[0]
            let buf = Array.zeroCreate<char> ary.Count
            for i = 0 to ary.Count - 1 do
                buf.[i] <- char (toInt ary.Storage.[i])
            ofString mm (String(buf)))

    add_func "charArrayOfString" (arrow ty_string (ty_array ty_char)) 1
        (fun mm argv ->
            let s = toString argv.[0]
            let v = array_create mm s.Length
            let ary = to_malarray v
            for i = 0 to s.Length - 1 do
                ary.Storage.[i] <- ofInt (int s.[i])
            ary.Count <- s.Length
            v)

    add_func "stringConcat" (arrow (ty_array ty_string) ty_string) 1
        (fun mm argv ->
            let ary = to_malarray argv.[0]
            let sb = StringBuilder()
            for i = 0 to ary.Count - 1 do
                sb.Append(toString ary.Storage.[i]) |> ignore
            ofString mm (sb.ToString()))

    add_func "stringSub" (arrow3 ty_string ty_int ty_int ty_string) 3
        (fun mm argv ->
            let s = toString argv.[0]
            let start = toInt argv.[1]
            let count = toInt argv.[2]
            try ofString mm (s.Substring(start, count))
            with _ -> mal_raise_Invalid_argument())

    add_func "stringStartWith" (Tarrow ("s", ty_string, Tarrow ("starting", ty_string, ty_bool))) 2
        (fun (mm : MemoryManager) (argv : MalValue array) ->
            let s = toString argv.[0]
            let starting = toString argv.[1]
            ofBool (s.StartsWith(starting)))

    add_func "stringEndWith" (Tarrow ("s", ty_string, Tarrow ("ending", ty_string, ty_bool))) 2
        (fun (mm : MemoryManager) (argv : MalValue array) ->
            let s = toString argv.[0]
            let ending = toString argv.[1]
            ofBool (s.EndsWith(ending)))
    
    add_func "stringToLower" ty_ss 1
        (fun (mm : MemoryManager) (argv : MalValue array) ->
            ofString mm ((toString argv.[0]).ToLowerInvariant()))

    add_func "stringIndexOf" (Tarrow ("s", ty_string, Tarrow ("pattern", ty_string, Tarrow ("startIndex", ty_int, ty_int)))) 3
        (fun (mm : MemoryManager) (argv : MalValue array) ->
            let s = toString argv.[0]
            let pattern = toString argv.[1]
            let startIndex = toInt argv.[2]
            try ofInt (s.IndexOf(pattern, startIndex))
            with _ -> mal_raise_Invalid_argument())

    add_func "pathGetExtension" ty_ss 1
        (fun (mm : MemoryManager) (argv : MalValue array) ->
            try
                ofString mm (System.IO.Path.GetExtension(toString argv.[0]))
            with _ -> mal_failwith mm "Path contains invalid character")

    let ty_array_add = arrow2 (ty_array ty_a) ty_a ty_unit
    let array_add_func (mm : MemoryManager) (argv : MalValue array) =
        array_add mm argv.[0] argv.[1]
        Value.unit
    add_func "arrayAdd" ty_array_add 2 array_add_func
    add_func "<<" ty_array_add 2 array_add_func

    add_func "arrayRemoveAt" (arrow2 (ty_array ty_a) ty_int ty_unit) 2
        (fun mm argv ->
            try
                array_remove_at mm argv.[0] (toInt argv.[1])
                Value.unit
            with :? IndexOutOfRangeException -> mal_raise_Index_out_of_range())

    add_func "arrayClear" (arrow (ty_array ty_a) ty_unit) 1
        (fun mm argv ->
            array_clear mm argv.[0]
            unit)
    
    add_func "memstat" (arrow ty_unit (Ttuple [ty_int; ty_int; ty_int])) 1
        (fun mm argv ->
            let process_memory_bytes = int (GC.GetTotalMemory(false))
            let mal_memory_bytes = System.Threading.Volatile.Read(&mm.counter)
            let percent = int (100.0 * float mal_memory_bytes / float mm.bytes_stop_exec)
            Value.blockCreate mm 0 [| ofInt process_memory_bytes; ofInt mal_memory_bytes; ofInt percent |])

    add_uu "collect" (fun mm -> collect())

    add_func "stringOfFloat" (arrow ty_float ty_string) 1 (fun mm argv -> ofString mm (Misc.string_of_float (toFloat argv.[0])))

    add_func "floatOfString" (arrow ty_string ty_float) 1
        (fun mm argv ->
            match Double.TryParse(toString argv.[0]) with
            | true, x -> ofFloat x
            | false, _ -> mal_failwith mm "floatOfString: Invalid argument")

    add_func "charOfInt" (arrow ty_int ty_char) 1
        (fun mm argv ->
            let i = toInt argv.[0]
            if int Char.MinValue <= i && i <= int Char.MaxValue
            then argv.[0]
            else mal_failwith mm "charOfInt: Out of range")

    add_func "intOfChar" (arrow ty_char ty_int) 1 (fun mm argv -> argv.[0])
    add_func "raise" (arrow ty_exn ty_a) 1 (fun mm argv -> raise (MalException argv.[0]))
    add_func "hash" (arrow ty_a ty_int) 1 (fun mm argv -> ofInt (MalCompare.hash argv.[0]))

    let interp = Interpreter(memory_manager_create_default(), tyenv, alloc, genv)
    let src = """
fun listAppend l1 l2 =
  case l1, l2 of
  | h1 :: t1, l2 -> h1 :: listAppend t1 l2
  | [], l2 -> l2;

fun optionMay f opt =
  case opt of
  | Some x -> f x
  | None -> ()

fun optionMap f opt =
  case opt of
  | Some x -> Some (f x)
  | None -> None

fun ref x = { contents = x }
fun refSet r x = r.contents <- x
fun refGet r = r.contents

fun arrayMap f a =
  begin
    val accu = [||];
    for i = 0 to arrayLength a - 1 do
      accu << f a.[i];
    accu
  end

fun arrayInit n f =
  begin
    val accu = [||];
    for i = 0 to n - 1 do
      accu << f i;
    accu
  end;

fun arrayIter (f : 'a -> unit) (a : 'a array) =
  for i = 0 to arrayLength a - 1 do
    f a.[i];

fun id x = x;
fun ignore x = ();
      
fun listLength l =
  case l of
  | [] -> 0
  | h :: t -> 1 + listLength t

fun listHead l = case l of h :: _ -> h

fun failwith msg = raise (Efailure msg)

fun min a b = if a < b then a else b
fun max a b = if a > b then a else b

// translated from stable_sort in https://github.com/ocaml/ocaml/blob/trunk/stdlib/array.ml   
fun sort cmp a =
  begin
    fun merge src1ofs src1len src2 src2ofs src2len dst dstofs =
      begin
        val src1r = src1ofs + src1len and src2r = src2ofs + src2len;
        fun loop i1 s1 i2 s2 d =
          if cmp s1 s2 <= 0 then
            begin
              dst.[d] <- s1;
              val i1 = i1 + 1;
              if i1 < src1r then
                loop i1 a.[i1] i2 s2 (d + 1)
              else
                arrayBlit src2 i2 dst (d + 1) (src2r - i2)
            end
          else
            begin
              dst.[d] <- s2;
              val i2 = i2 + 1;
              if i2 < src2r then
                loop i1 s1 i2 src2.[i2] (d + 1)
              else
                arrayBlit a i1 dst (d + 1) (src1r - i1)
            end;
        loop src1ofs a.[src1ofs] src2ofs src2.[src2ofs] dstofs;
      end;
    fun isortto srcofs dst dstofs len =
      for i = 0 to len - 1 do
        begin
          val e = a.[srcofs + i];
          var j = dstofs + i - 1;
          while (j >= dstofs && cmp dst.[j] e > 0) do
            begin
              dst.[j + 1] <- dst.[j];
              j <- j - 1;
            end;
          dst.[j + 1] <- e;
        end;
    val cutoff = 5;
    fun sortto srcofs dst dstofs len =
      if len <= cutoff then isortto srcofs dst dstofs len
      else
        begin
          val l1 = len / 2;
          val l2 = len - l1;
          sortto (srcofs + l1) dst (dstofs + l1) l2;
          sortto srcofs a (srcofs + l2) l1;
          merge (srcofs + l2) l1 dst (dstofs + l1) l2 dst dstofs;
        end;
    val l = arrayLength a;
    if l <= cutoff then isortto 0 a 0 l
    else
      begin
        val l1 = l / 2;
        val l2 = l - l1;
        val t = arrayCreate l2 a.[0];
        sortto l1 t 0 l2;
        sortto 0 a l2 l1;
        merge l2 l1 t 0 l2 a 0;
      end
  end;

fun lexbufCreate s = { lbSource = s, lbStartPos = 0, lbEndPos = 0, lbScanStartPos = 0, lbEof = false };
fun sprintf fmt = kprintf (fn s -> s) fmt;
fun sprintfn fmt = kprintf (fn s -> s ^ "\r\n") fmt;
"""
    interp.Do(src)
    if interp.State <> State.Success then
        dontcare()
    interp.Alias "@" "listAppend"
    interp.Alias "!" "refGet"
    interp.Alias ":=" "refSet"
    Typechk.tyenv_clone interp.Tyenv, interp.Alloc, interp.GEnv

let createInterpreterExt(mm) = Interpreter(mm, tyenv_clone tyenv_std, alloc_std.Clone(), Array.copy genv_std)
let createInterpreter() = createInterpreterExt(memory_manager_create_default())
