module FsMiniMAL.MalCompare

open System
open System.Collections.Generic

open Value

type Mode =
    | Compare
    | Equal
    | Not_equal
    | Less_than
    | Greater_than
    | Less_equal
    | Greater_equal

type Frame = { v0 : MalValue; v1 : MalValue; mutable i : int }

let [<Literal>] Uncomparable = 0x80000000
let [<Literal>] Nan          = 0x80000001

let initial : Frame array = [||]

type MalCompare(mm : MemoryManager, mode : Mode, argv : MalValue array) =
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
            accu <- compare (toInt v0) (toInt v1)
        | MalValueKind.INT, MalValueKind.BLOCK ->
            accu <- compare (toInt v0) (getTag v1)
        | MalValueKind.BLOCK, MalValueKind.INT ->
            accu <- compare (getTag v0) (toInt v1)
        | MalValueKind.FLOAT, MalValueKind.FLOAT ->
            let x0 = toFloat v0
            let x1 = toFloat v1
            if mode = Compare then
                accu <- x0.CompareTo(x1)
            else
                if Double.IsNaN x0 || Double.IsNaN x1 then
                    accu <- Nan
                else
                    accu <- x0.CompareTo(x1)
        | MalValueKind.STRING, MalValueKind.STRING ->
            accu <- Math.Sign(String.CompareOrdinal(toString v0, toString v1))
        | MalValueKind.BLOCK, MalValueKind.BLOCK -> 
            let d = compare (getTag v0) (getTag v1)
            if d <> 0 then
                accu <- d
            else
                stack_push { v0 = v0; v1 = v1; i = 0 }
        | MalValueKind.ARRAY, MalValueKind.ARRAY ->
            let ary0 = v0 :?> MalArray
            let ary1 = v1 :?> MalArray
            let d = compare ary0.Count ary1.Count
            if d <> 0 || ary0.Count = 0  then
                accu <- d
            else
                stack_push { v0 = v0; v1 = v1; i = 0 }
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
            match frame.v0.Kind, frame.v1.Kind with
            | MalValueKind.BLOCK, MalValueKind.BLOCK ->
                let fields0 = getFields frame.v0
                let fields1 = getFields frame.v1
                if frame.i < fields0.Length - 1 then
                    start fields0.[frame.i] fields1.[frame.i]
                    frame.i <- frame.i + 1
                else
                    // For the final value of a block, continue without growing the stack.
                    // This is required to compare long list which has a length of more than maximum stack depth.
                    stack_discard_top()
                    start fields0.[frame.i] fields1.[frame.i]
            | MalValueKind.ARRAY, MalValueKind.ARRAY ->
                let ary0 = frame.v0 :?> MalArray
                let ary1 = frame.v1 :?> MalArray
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
            | Compare -> ofCompare accu
            | Equal -> ofBool (accu = 0)
            | Not_equal -> ofBool (not (accu = 0))
            | Less_than -> ofBool (accu = -1)
            | Greater_than -> ofBool (accu = 1)
            | Less_equal -> ofBool (accu = -1 || accu = 0)
            | Greater_equal -> ofBool (accu = 0 || accu = 1)

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
        | MalValueKind.INT -> combine (Value.toInt v)
        | MalValueKind.FLOAT -> combine ((Value.toFloat v).GetHashCode())
        | MalValueKind.STRING -> combine ((Value.toString v).GetHashCode())
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
