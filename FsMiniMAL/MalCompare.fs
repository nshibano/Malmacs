module FsMiniMAL.MalCompare

open System
open System.Collections.Generic
open System.Diagnostics

open Value

type Mode =
    | Compare
    | Equal
    | Not_equal
    | Less_than
    | Greater_than
    | Less_equal
    | Greater_equal

type Frame = { a : value; b : value; mutable i : int }

let [<Literal>] Uncomparable = 0x80000000
let [<Literal>] Nan          = 0x80000001

let initial : Frame array = [||]

type MalCompare(mm : memory_manager, mode : Mode, argv : value array) =
    let mutable accu = 0
    let mutable stack_topidx = -1
    let mutable stack = initial
    let mutable message : string = null

    let stack_push frame =
        stack_topidx <- stack_topidx + 1
        stack <- array_ensure_capacity_exn mm.maximum_compare_stack_depth (stack_topidx + 1) stack
        stack.[stack_topidx] <- frame

    let stack_discard_top() =
        stack.[stack_topidx] <- Unchecked.defaultof<Frame>
        stack_topidx <- stack_topidx - 1

    let start a b =
        match a, b with
        | Vint (i0, _), Vint (i1, _)
        | Vint (i0, _),  Vblock (i1, _, _)
        | Vblock (i0, _, _), Vint (i1, _) ->
            accu <- compare i0 i1
        | Vfloat (x0, _), Vfloat (x1, _) ->
            if mode = Compare then
                accu <- x0.CompareTo(x1)
            else
                if Double.IsNaN x0 || Double.IsNaN x1 then
                    accu <- Nan
                else
                    accu <- x0.CompareTo(x1)
        | Vstring (s0, _), Vstring (s1, _) ->
            accu <- Math.Sign(String.CompareOrdinal(s0, s1))
        | Vblock (tag0, _, _), Vblock (tag1, _, _) ->
            let d = compare tag0 tag1
            if d <> 0 then
                accu <- d
            else
                stack_push { a = a; b = b; i = 0 }
        | Varray ary0, Varray ary1 ->
            let d = compare ary0.count ary1.count
            if d <> 0 || ary0.count = 0  then
                accu <- d
            else
                stack_push { a = a; b = b; i = 0 }
        | (Vfunc _ | Vkfunc _ | Vclosure _ | Vpartial _ | Vcoroutine _), (Vfunc _ | Vkfunc _ | Vclosure _ | Vpartial _ | Vcoroutine _) ->
            accu <- Uncomparable
            message <- "functional value"
        | Vobj _, Vobj _ ->
            accu <- Uncomparable
            message <- "Uncomparable object"
        | _ -> dontcare()
    
    let isFinished() = stack_topidx = -1 || accu <> 0

    let run (sliceTicks : int) =
        let tickCountAtStart = Environment.TickCount
        while (Environment.TickCount - tickCountAtStart < sliceTicks) && not (isFinished()) do
            let frame = stack.[stack_topidx]
            match frame.a, frame.b with
            | Vblock (_, fields0, _), Vblock (_, fields1, _) ->
                if frame.i < fields0.Length - 1 then
                    start fields0.[frame.i] fields1.[frame.i]
                    frame.i <- frame.i + 1
                else
                    // For the final value of a block, continue without growing the stack.
                    // This is required to compare long list which has a length of more than maximum stack depth.
                    stack_discard_top()
                    start fields0.[frame.i] fields1.[frame.i]
            | Varray ary0, Varray ary1 ->
                if frame.i < ary0.count then
                    start ary0.storage.[frame.i] ary1.storage.[frame.i]
                    frame.i <- frame.i + 1
                else stack_discard_top()
            | _ -> dontcare()

            if stack_topidx + 1 >= mm.maximum_compare_stack_depth then
                accu <- Uncomparable
                message <- "stack overflow"
    
    let result() =
        if accu = Uncomparable then
            mal_failwith mm ((match mode with Compare -> "compare: " | _ -> "equal: ") + message)
        else
            match mode with
            | Compare -> of_compare accu
            | Equal -> of_bool (accu = 0)
            | Not_equal -> of_bool (not (accu = 0))
            | Less_than -> of_bool (accu = -1)
            | Greater_than -> of_bool (accu = 1)
            | Less_equal -> of_bool (accu = -1 || accu = 0)
            | Greater_equal -> of_bool (accu = 0 || accu = 1)

    do start argv.[0] argv.[1]

    interface IMalCoroutine with
        member this.IsFinished = isFinished()
        member this.Run sliceTicks = run sliceTicks
        member this.Result = result()
        member this.Dispose() = ()

let hash v =
    let mutable limit = 10

    let queue = Queue<value>()        
    queue.Enqueue(v)

    let mutable accu = 0
    let combine h =
        accu <- (accu * 23) + h

    while queue.Count > 0 do
        let v = queue.Dequeue()
        match v with
        | Vint (i, _) -> combine i
        | Vfloat (x, _) -> combine (x.GetHashCode())
        | Vstring (s, _) -> combine (s.GetHashCode())
        | Vblock (tag, fields, _) ->
            combine tag
            let n = min fields.Length limit
            for i = 0 to n - 1 do
                queue.Enqueue(fields.[i])
            limit <- limit - n
        | Varray ary ->
            combine ary.count
            let n = min ary.count limit
            for i = 0 to n - 1 do
                queue.Enqueue(ary.storage.[i])
            limit <- limit - n
        | Vfunc _
        | Vkfunc _
        | Vcoroutine _
        | Vclosure _
        | Vpartial _
        | Vobj _ -> combine (LanguagePrimitives.PhysicalHash v)       
        | Vvar _ -> dontcare()

    accu
