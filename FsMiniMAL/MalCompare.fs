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

type Frame = { a : MalValue; b : MalValue; mutable i : int }

let [<Literal>] Uncomparable = 0x80000000
let [<Literal>] Nan          = 0x80000001

let initial : Frame array = [||]

type MalCompare(mm : memory_manager, mode : Mode, argv : MalValue array) =
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

    let start (v0 : MalValue) (v1 : MalValue) =
        match v0.Kind, v1.Kind with
        | MalValueKind.INT, MalValueKind.INT ->
            accu <- compare (v0 :?> MalInt).Get (v1 :?> MalInt).Get
        | MalValueKind.INT, MalValueKind.BLOCK ->
            accu <- compare (v0 :?> MalInt).Get (v1 :?> MalBlock).Tag
        | MalValueKind.BLOCK, MalValueKind.INT ->
            accu <- compare (v0 :?> MalBlock).Tag (v1 :?> MalInt).Get
        | MalValueKind.FLOAT, MalValueKind.FLOAT ->
            let x0 = (v0 :?> MalFloat).Get
            let x1 = (v1 :?> MalFloat).Get
            if mode = Compare then
                accu <- x0.CompareTo(x1)
            else
                if Double.IsNaN x0 || Double.IsNaN x1 then
                    accu <- Nan
                else
                    accu <- x0.CompareTo(x1)
        | MalValueKind.STRING, MalValueKind.STRING ->
            let s0 = (v0 :?> MalString).Get
            let s1 = (v1 :?> MalString).Get
            accu <- Math.Sign(String.CompareOrdinal(s0, s1))
        | MalValueKind.BLOCK, MalValueKind.BLOCK -> 
            let tag0 = (v0 :?> MalBlock).Tag
            let tag1 = (v1 :?> MalBlock).Tag
            let d = compare tag0 tag1
            if d <> 0 then
                accu <- d
            else
                stack_push { a = v0; b = v1; i = 0 }
        | MalValueKind.ARRAY, MalValueKind.ARRAY ->
            let ary0 = v0 :?> MalArray
            let ary1 = v1 :?> MalArray
            let d = compare ary0.Count ary1.Count
            if d <> 0 || ary0.Count = 0  then
                accu <- d
            else
                stack_push { a = v0; b = v1; i = 0 }
        | (MalValueKind.FUNC|MalValueKind.KFUNC|MalValueKind.PARTIAL|MalValueKind.CLOSURE|MalValueKind.COROUTINE),
          (MalValueKind.FUNC|MalValueKind.KFUNC|MalValueKind.PARTIAL|MalValueKind.CLOSURE|MalValueKind.COROUTINE) ->
            accu <- Uncomparable
            message <- "functional value"
        | MalValueKind.OBJ, MalValueKind.OBJ ->
            accu <- Uncomparable
            message <- "Uncomparable object"
        | _ -> dontcare()
    
    let isFinished() = stack_topidx = -1 || accu <> 0

    let run (sliceTicks : int) =
        let tickCountAtStart = Environment.TickCount
        while (Environment.TickCount - tickCountAtStart < sliceTicks) && not (isFinished()) do
            let frame = stack.[stack_topidx]
            match frame.a.Kind, frame.b.Kind with
            | MalValueKind.BLOCK, MalValueKind.BLOCK ->
                let fields0 = (frame.a :?> MalBlock).Fields
                let fields1 = (frame.b :?> MalBlock).Fields
                if frame.i < fields0.Length - 1 then
                    start fields0.[frame.i] fields1.[frame.i]
                    frame.i <- frame.i + 1
                else
                    // For the final value of a block, continue without growing the stack.
                    // This is required to compare long list which has a length of more than maximum stack depth.
                    stack_discard_top()
                    start fields0.[frame.i] fields1.[frame.i]
            | MalValueKind.ARRAY, MalValueKind.ARRAY ->
                let ary0 = frame.a :?> MalArray
                let ary1 = frame.b :?> MalArray
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

    let queue = Queue<MalValue>()        
    queue.Enqueue(v)

    let mutable accu = 0
    let combine h =
        accu <- (accu * 23) + h

    while queue.Count > 0 do
        let v = queue.Dequeue()
        match v.Kind with
        | MalValueKind.INT -> combine (Value.to_int v)
        | MalValueKind.FLOAT -> combine ((Value.to_float v).GetHashCode())
        | MalValueKind.STRING -> combine ((Value.to_string v).GetHashCode())
        | MalValueKind.BLOCK ->
            let block = v :?> MalBlock
            combine block.Tag
            let n = min block.Fields.Length limit
            for i = 0 to n - 1 do
                queue.Enqueue(block.Fields.[i])
            limit <- limit - n
        | MalValueKind.ARRAY ->
            let ary = v :?> MalArray
            combine ary.Count
            let n = min ary.Count limit
            for i = 0 to n - 1 do
                queue.Enqueue(ary.Storage.[i])
            limit <- limit - n
        | MalValueKind.FUNC
        | MalValueKind.KFUNC
        | MalValueKind.PARTIAL
        | MalValueKind.CLOSURE
        | MalValueKind.COROUTINE
        | MalValueKind.OBJ -> combine (LanguagePrimitives.PhysicalHash v)       
        | MalValueKind.VAR -> dontcare()
        | _ -> dontcare()

    accu
