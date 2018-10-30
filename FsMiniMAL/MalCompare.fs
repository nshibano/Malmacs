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

    let start (v0 : value) (v1 : value) =
        match v0.Kind, v1.Kind with
        | ValueKind.VKint, ValueKind.VKint ->
            accu <- compare (v0 :?> Int).Get (v1 :?> Int).Get
        | ValueKind.VKint, ValueKind.VKblock ->
            accu <- compare (v0 :?> Int).Get (v1 :?> Block).Tag
        | ValueKind.VKblock, ValueKind.VKint ->
            accu <- compare (v0 :?> Block).Tag (v1 :?> Int).Get
        | ValueKind.VKfloat, ValueKind.VKfloat ->
            let x0 = (v0 :?> Float).Get
            let x1 = (v1 :?> Float).Get
            if mode = Compare then
                accu <- x0.CompareTo(x1)
            else
                if Double.IsNaN x0 || Double.IsNaN x1 then
                    accu <- Nan
                else
                    accu <- x0.CompareTo(x1)
        | ValueKind.VKstring, ValueKind.VKstring ->
            let s0 = (v0 :?> String).Get
            let s1 = (v1 :?> String).Get
            accu <- Math.Sign(String.CompareOrdinal(s0, s1))
        | ValueKind.VKblock, ValueKind.VKblock -> 
            let tag0 = (v0 :?> Block).Tag
            let tag1 = (v1 :?> Block).Tag
            let d = compare tag0 tag1
            if d <> 0 then
                accu <- d
            else
                stack_push { a = v0; b = v1; i = 0 }
        | ValueKind.VKarray, ValueKind.VKarray ->
            let ary0 = v0 :?> Array
            let ary1 = v1 :?> Array
            let d = compare ary0.Count ary1.Count
            if d <> 0 || ary0.Count = 0  then
                accu <- d
            else
                stack_push { a = v0; b = v1; i = 0 }
        | (ValueKind.VKfunc|ValueKind.VKkfunc|ValueKind.VKpartial|ValueKind.VKclosure|ValueKind.VKcoroutine),
          (ValueKind.VKfunc|ValueKind.VKkfunc|ValueKind.VKpartial|ValueKind.VKclosure|ValueKind.VKcoroutine) ->
            accu <- Uncomparable
            message <- "functional value"
        | ValueKind.VKobj, ValueKind.VKobj ->
            accu <- Uncomparable
            message <- "Uncomparable object"
        | _ -> dontcare()
    
    let isFinished() = stack_topidx = -1 || accu <> 0

    let run (sliceTicks : int) =
        let tickCountAtStart = Environment.TickCount
        while (Environment.TickCount - tickCountAtStart < sliceTicks) && not (isFinished()) do
            let frame = stack.[stack_topidx]
            match frame.a.Kind, frame.b.Kind with
            | ValueKind.VKblock, ValueKind.VKblock ->
                let fields0 = (frame.a :?> Block).Fields
                let fields1 = (frame.b :?> Block).Fields
                if frame.i < fields0.Length - 1 then
                    start fields0.[frame.i] fields1.[frame.i]
                    frame.i <- frame.i + 1
                else
                    // For the final value of a block, continue without growing the stack.
                    // This is required to compare long list which has a length of more than maximum stack depth.
                    stack_discard_top()
                    start fields0.[frame.i] fields1.[frame.i]
            | ValueKind.VKarray, ValueKind.VKarray ->
                let ary0 = frame.a :?> Array
                let ary1 = frame.b :?> Array
                if frame.i < ary0.Count then
                    start ary0.Storage.[frame.i] ary1.Storage.[frame.i]
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
        match v.Kind with
        | ValueKind.VKint -> combine (Value.to_int v)
        | ValueKind.VKfloat -> combine ((Value.to_float v).GetHashCode())
        | ValueKind.VKstring -> combine ((Value.to_string v).GetHashCode())
        | ValueKind.VKblock ->
            let block = v :?> Block
            combine block.Tag
            let n = min block.Fields.Length limit
            for i = 0 to n - 1 do
                queue.Enqueue(block.Fields.[i])
            limit <- limit - n
        | ValueKind.VKarray ->
            let ary = v :?> Array
            combine ary.Count
            let n = min ary.Count limit
            for i = 0 to n - 1 do
                queue.Enqueue(ary.Storage.[i])
            limit <- limit - n
        | ValueKind.VKfunc
        | ValueKind.VKkfunc
        | ValueKind.VKpartial
        | ValueKind.VKclosure
        | ValueKind.VKcoroutine
        | ValueKind.VKobj -> combine (LanguagePrimitives.PhysicalHash v)       
        | ValueKind.VKvar -> dontcare()
        | _ -> dontcare()

    accu
