namespace FsMiniMAL

open System
open System.Collections.Generic
open System.Text

open FsMiniMAL.Lexing
open Syntax
open Types
open Typechk
open Value

type Tag =
    | Start = 0

    | Block_Eval = 1
    
    | Array_Eval = 1
    
    | BlockWith_EvalOrig = 1
    | BlockWith_EvalSets = 2
    
    | Apply_Eval = 1
    | Apply_Apply = 2
    | Apply_Closure = 3
    | Apply_Coroutine = 4

    | Tailcall_Eval = 1
    
    | Begin_Eval = 1
    
    | Case_EvalTest = 1
    | Case_Match = 2
    | Case_EvalWhen = 3
    | Case_EvalResult = 4
    
    | Try_EvalTry = 1
    | Try_EvalCatch = 2
    
    | If_EvalTest = 1
    | If_EvalResult = 2
    
    | Setenvvar_Eval = 1
    
    | Setcapturevar_Eval = 1

    | Forloop_EvalFirst = 1
    | Forloop_EvalLast = 2
    | Forloop_To = 3
    | Forloop_Downto = 4
    
    | While_EvalCond = 1
    | While_EvalBody = 2
    
    | Val_Eval = 1
    
    | Var_Eval = 1

    | Compare_CompareFields = 1
    | ComparisonOperators_CallCompare = 1

    | TopExpr_Eval = 1

    | TopValVarFun_Eval = 1

type StackItem =
    struct
        val mutable code : code
        val mutable frame : obj
    end

type BlockFrame = { mutable pc : Tag; mutable fields : MalValue array; mutable i : int }
type ArrayFrame = { mutable pc : Tag; mutable ary : MalValue; mutable i : int }
type BlockwithFrame = { mutable pc : Tag; mutable fields : MalValue array; mutable i : int; mutable tag : int }
type ApplyFrame = { mutable pc : Tag; mutable i : int; mutable values : MalValue array; mutable oldenv : MalValue array; mutable oldcaptures : MalValue array; mutable calling_code : code }
type TailcallFrame = { mutable pc : Tag; mutable args : MalValue array; mutable i : int }
type BeginFrame = { mutable pc : Tag; mutable i : int }
type CaseFrame = { mutable pc : Tag; mutable testvalue : MalValue; mutable i : int }
type ForloopFrame = { mutable pc : Tag; mutable i : int; mutable j : int }
type ValVarFrame = { mutable pc : Tag; mutable i : int }

type State =
    | Running = 0
    | Clear = 1
    | Success = 2
    | Failure = 3
    | Paused = 5

type Interpreter(mm : MemoryManager, tyenv : tyenv, alloc : alloc, env : MalValue array) =

    let mutable state = State.Clear
    let mutable accu : MalValue = unit
    let mutable env : MalValue array = env
    let mutable captures : MalValue array = null
    let mutable stack = Array.zeroCreate<StackItem> 16
    let mutable stack_topidx = -1
    let mutable tyenv = tyenv
    let mutable alloc = alloc
    let mutable latestMessage : Message option = None
    let mutable cycles = 0L
    let mutable message_hook : Message -> unit = ignore
    let mutable store : obj = null

    let set (name : string) (value : MalValue) (ty : type_expr) (access : access) =
        tyenv <- Types.add_value tyenv name { vi_access = access; vi_type = ty }
        let ofs = alloc.Add(name, access)
        env <- array_ensure_capacity_exn mm.maximum_array_length (ofs + 1) env
        env.[ofs] <-
            match access with
            | Immutable -> value
            | Mutable -> MalVar(value) :> MalValue
        ofs

    let stack_push (code : code) (frame : obj) =
        stack_topidx <- stack_topidx + 1
        if (stack_topidx + 1) > stack.Length then
            let new_stack = Array.zeroCreate (2 * stack.Length)
            Array.blit stack 0 new_stack 0 stack.Length
            stack <- new_stack
        stack.[stack_topidx].code <- code
        stack.[stack_topidx].frame <- frame

    let stack_discard_top() =
        match stack.[stack_topidx].code with
        | UEapply _ ->
            let frame = stack.[stack_topidx].frame :?> ApplyFrame
            if frame.pc = Tag.Apply_Closure then
                env <- frame.oldenv
                captures <- frame.oldcaptures
        | UEcoroutine ->
            let co = stack.[stack_topidx].frame :?> IMalCoroutine
            co.Dispose()
        | _ -> ()
        stack.[stack_topidx].code <- Unchecked.defaultof<code>
        stack.[stack_topidx].frame <- null
        stack_topidx <- stack_topidx - 1
        
    let clear() =
        state <- State.Clear
        accu <- Value.unit
        latestMessage <- None
        while stack_topidx > -1 do
            stack_discard_top()

    let send_message (msg : Message) =
        latestMessage <- Some msg
        message_hook msg

    let print_new_values new_values = 
        let new_values = List.map (fun (name, info) ->
            let var_info = alloc.Get(name)
            let value =
                match var_info.access with
                | Immutable -> env.[var_info.ofs]
                | Mutable -> (env.[var_info.ofs] :?> MalVar).Content
            (name, value, info)) new_values
        send_message (Message.NewValues (tyenv, new_values))
    
    let start_code (c : code) =
        match c with
        | UEconst v ->
            accu <- v
        | UEgetenv ofs ->
            accu <- env.[ofs]
        | UEgetenvvar ofs ->
            accu <- (env.[ofs] :?> MalVar).Content
        | UEgetcap ofs ->
            accu <- captures.[ofs]
        | UEgetcapvar ofs ->
            accu <- (captures.[ofs] :?> MalVar).Content
        | UEfn (env_size, argc, is_capture_flags, ofss, body) ->
            let capture_values = Array.zeroCreate<MalValue> is_capture_flags.Length
            for i = 0 to is_capture_flags.Length - 1 do
                let ofs = ofss.[i]
                capture_values.[i] <- if is_capture_flags.[i] then captures.[ofs] else env.[ofs] 
            accu <- MalClosure(argc, env_size, capture_values, body) //captures = capture_values, arity = argc, env_size = env_size, code = body)
        | UCfun defs ->
            // Doing this in two steps because recursive function captures itself from env.
            // Step 1: create closures with captures unfilled, and set it to env.
            for ofs, fn in defs do
                match fn with
                | UEfn (env_size, argc, is_capture_flags, _, body) ->
                    env.[ofs] <- MalClosure(argc, env_size, Array.zeroCreate<MalValue> is_capture_flags.Length, body) :> MalValue
                | _ -> dontcare()
            // Step 2: fill captures.
            for ofs, fn in defs do
                match fn with
                | UEfn (is_capture_flags = is_capture_flags; ofss = ofss) ->
                    let capture_values = (env.[ofs] :?> MalClosure).Captures
                    for i = 0 to is_capture_flags.Length - 1 do
                        let ofs = ofss.[i]
                        capture_values.[i] <- if is_capture_flags.[i] then captures.[ofs] else env.[ofs]
                | _ -> dontcare()
        | UTCtype (dl, tyenv') -> 
            tyenv <- tyenv'
            message_hook (Message.TypeDefined dl)
        | UTChide (name, tyenv', alloc') ->
            tyenv <- tyenv'
            alloc <- alloc'
            message_hook (Message.TypeHidden name)
        | UTCremove (name, ofs, tyenv', alloc') ->
            env.[ofs] <- Value.zero
            tyenv <- tyenv'
            alloc <- alloc'
            message_hook (Message.ValueRemoved name)
        | UTCexn (name, tyenv', alloc') ->
            tyenv <- tyenv'
            alloc <- alloc'
            message_hook (Message.ExceptionDefined name)
        | UTClex (defs, tyenv', alloc', shadowed, new_values) ->
            // Step 1
            let actionss =
                Array.map (fun (arity, ofs, alphabets, dfa, codes : code array) ->
                    let closures = Array.zeroCreate<MalValue> codes.Length
                    let lexer_obj : int * HashSet<int> * DfaNode * MalValue array = (arity, alphabets, dfa, closures)
                    let lexing2 (mm : MemoryManager) (frame : MalValue array) =
                        let arity, alphabets, dfa, closures = (frame.[1] :?> MalObj).Obj :?>  int * HashSet<int> * DfaNode * MalValue array
                        let lexbuf = frame.[2 + arity - 1]
                        let lexbuf_fields = getFields lexbuf
                        let source = toString lexbuf_fields.[0]
                        let scan_start_pos = toInt lexbuf_fields.[3]
                        let eof = toBool lexbuf_fields.[4]
                        if (scan_start_pos = source.Length && eof) || (scan_start_pos > source.Length) then
                            mal_raise_Match_failure ()
                        else
                            match MalLex.Exec alphabets dfa source scan_start_pos with
                            | Some (end_pos, action_idx, eof) ->
                                lexbuf_fields.[1] <- ofInt scan_start_pos
                                lexbuf_fields.[2] <- ofInt end_pos
                                lexbuf_fields.[3] <- ofInt end_pos
                                lexbuf_fields.[4] <- ofBool eof
                                Array.append [| closures.[action_idx] |] (Array.sub frame 2 (frame.Length - 2))
                            | None ->
                                mal_raise_Match_failure ()
                    env.[ofs] <- MalPartial(arity, [| MalKFunc (0, lexing2); MalObj(lexer_obj) |]) :> MalValue
                    (codes, closures)) defs
            // Step 2
            for codes, closures in actionss do
                for i = 0 to codes.Length - 1 do
                    match codes.[i] with
                    | UEfn (env_size, arity, is_capture_flags, ofss, code) ->
                        let capture_values = Array.zeroCreate<MalValue> is_capture_flags.Length
                        for j = 0 to is_capture_flags.Length - 1 do
                            let ofs = ofss.[j]
                            capture_values.[j] <- if is_capture_flags.[j] then captures.[ofs] else env.[ofs]
                        closures.[i] <- MalClosure(arity, env_size, capture_values, code) :> MalValue
                    | _ -> dontcare()
            tyenv <- tyenv'
            alloc <- alloc'
            for ofs in shadowed do
                env.[ofs] <- Value.zero
            print_new_values new_values
        | UTCupd (tyenv', alloc') ->
            tyenv <- tyenv'
            alloc <- alloc'
        | UEblock _ ->
            stack_push c { BlockFrame.pc = Tag.Start; fields = null; i = 0 }
        | UEarray _ ->
            stack_push c { ArrayFrame.pc = Tag.Start; ary = Unchecked.defaultof<MalValue>; i = 0 }
        | UEblockwith _ ->
            stack_push c { BlockwithFrame.pc = Tag.Start; fields = null; i = 0; tag = 0 }
        | UEapply _ ->
            stack_push c { ApplyFrame.pc = Tag.Start; i = 0; values = null; oldenv = null; oldcaptures = null; calling_code = Unchecked.defaultof<code> }
        | UEtailcall _ ->
            stack_push c { TailcallFrame.pc = Tag.Start; args = null; i = 0 }
        | UEbegin _ ->
            stack_push c { BeginFrame.pc = Tag.Start; i = 0 }
        | UEcase _ ->
            stack_push c { CaseFrame.pc = Tag.Start; testvalue = Unchecked.defaultof<MalValue>; i = 0 }
        | UEfor _ ->
            stack_push c { ForloopFrame.pc = Tag.Start; i = 0; j = 0 }
        | UCval _
        | UCvar _ ->
            stack_push c { ValVarFrame.pc = Tag.Start; i = 0 }
        | UEtry _
        | UEif _
        | UEsetenvvar _
        | UEsetcapvar _
        | UEwhile _
        | UTCexpr _
        | UTCvalvarfun _ ->
            stack_push c (ref Tag.Start)
        | UEcoroutine -> dontcare()

    let parse (src : string) =
        let lexbuf = LexBuffer<char>.FromString src
        lexbuf.EndPos <- { lexbuf.EndPos with pos_fname = dummy_file_name }
        lexbuf.BufferLocalStore.["src"] <- src
        try
            let cmds, _ = Parser.Program Lexer.main lexbuf
            Ok cmds
        with
        | LexHelper.Lexical_error lex_err ->
            let loc = { src = src; st = lexbuf.StartPos; ed = lexbuf.EndPos }
            Error (LexicalError (lex_err, loc))
        | Failure "parse error" ->
            let loc = { src = src; st = lexbuf.StartPos; ed = lexbuf.EndPos }
            Error (SyntaxError loc)
    
    let start src =
        clear()
        match parse src with
        | Error msg ->
            state <- State.Failure
            send_message msg
        | Ok cmds ->
            let warning_sink (err : type_error_desc, loc : location) =
                send_message (Message.TypeError (err, loc))
                
            let tyenvs =
                try Some (Typechk.type_command_list warning_sink tyenv cmds)
                with Type_error (err, loc) ->
                    state <- State.Failure
                    send_message (Message.TypeError (err, loc))
                    None
            match tyenvs with
            | None -> ()
            | Some tyenvs ->
                let genv_size, tcmds = Translate.translate_top_command_list alloc tyenvs (Array.ofList cmds)
                if genv_size > mm.maximum_array_length then
                    state <- State.Failure
                    send_message (UncatchableException "Env size limit is exceeded")
                else
                    env <- array_ensure_capacity_exn mm.maximum_array_length genv_size env
                    start_code (UEbegin tcmds)
                    state <- State.Running
        
    let rec do_match (bind : bool) (p : pattern) (x : MalValue) =
        match p, x.Kind with
        | UPid ofs, _ ->
            if bind then env.[ofs] <- x
            true
        | UPas (p, ofs), _ ->
            if bind then env.[ofs] <- x
            do_match bind p x
        | UPor (p1, p2), _ -> do_match bind p1 x || do_match bind p2 x
        | UPint i1, MalValueKind.INT -> i1 = (x :?> MalInt).Get
        | UPfloat x1, MalValueKind.FLOAT -> x1 = (x :?> MalFloat).Get
        | UPstring s1, MalValueKind.STRING -> s1 = (x :?> MalString).Get
        | UPblock (pattag, patl), MalValueKind.BLOCK ->
            let block = x :?> MalBlock
            if pattag = block.Tag && patl.Length = block.Fields.Length then
                let mutable cont = true
                let mutable i = 0
                while cont && i < patl.Length do
                    cont <- do_match bind patl.[i] block.Fields.[i]
                    i <- i + 1
                cont
            else false
        | UParray patl, MalValueKind.ARRAY ->
            let ary = x :?> MalArray
            if patl.Length = ary.Count then
                let mutable cont = true
                let mutable i = 0
                while cont && i < patl.Length do
                    cont <- do_match bind patl.[i] (array_get dummy_mm x i)
                    i <- i + 1
                cont
            else false
        | UPany, _ -> true
        | UPint _, MalValueKind.BLOCK
        | UPblock _, MalValueKind.INT -> false
        | _ -> dontcare()
    
    let do_raise (exn_value : MalValue) =
        let rec case_loop (i : int) (cases : (pattern * code) array) (j : int) =
            if j < cases.Length then
                let pat, handler = cases.[j]
                if do_match false pat exn_value then
                    Some (i, pat, handler)
                else case_loop i cases (j + 1)
            else None
        
        let rec frame_loop i =
            if i >= 0 then
                match stack.[i].code with
                | UEtry (_, catchl) ->
                    let frame = stack.[i].frame :?> ref<Tag>
                    if !frame = Tag.Try_EvalTry then
                        case_loop i catchl 0
                    else frame_loop (i - 1)
                | _ -> frame_loop (i - 1)
            else None
        match frame_loop stack_topidx with
        | Some (i, pat, catch) ->
            while stack_topidx > i do
                stack_discard_top()
            let frame = stack.[i].frame :?> ref<Tag>
            frame := Tag.Try_EvalCatch
            do_match true pat exn_value |> ignore
            start_code catch
        | None ->
            state <- State.Failure
            send_message (UncaughtException (tyenv, exn_value))

    let run (sliceTicks : int) =
        
        if state = State.Paused then
            state <- State.Running
        
        let tickCountAtStart = Environment.TickCount
        
        while state = State.Running && (Environment.TickCount - tickCountAtStart) < sliceTicks do
            let code = stack.[stack_topidx].code
            let frame = stack.[stack_topidx].frame
            match code with
            | UEblock (tag, field_exprs) ->
                let frame = frame :?> BlockFrame
                match frame.pc with
                | Tag.Start ->
                    frame.pc <- Tag.Block_Eval
                    frame.fields <- Array.zeroCreate<MalValue> field_exprs.Length
                    start_code field_exprs.[0]
                | Tag.Block_Eval ->
                    frame.fields.[frame.i] <- accu
                    frame.i <- frame.i + 1
                    if frame.i < field_exprs.Length then
                        start_code field_exprs.[frame.i]
                    else
                        accu <- blockCreate mm tag frame.fields
                        stack_discard_top()
                | _ -> dontcare()
            | UEarray fields ->
                let frame = frame :?> ArrayFrame
                match frame.pc with
                | Tag.Start ->
                    try
                        frame.ary <- array_create mm fields.Length
                        if fields.Length = 0 then
                            accu <- frame.ary
                            stack_discard_top()
                        else
                            frame.pc <- Tag.Array_Eval
                            start_code fields.[0]
                    with
                        | MalUncatchableException msg ->
                            state <- State.Failure
                            send_message (Message.UncatchableException msg)
                | Tag.Array_Eval ->
                    array_add mm frame.ary accu
                    frame.i <- frame.i + 1
                    if frame.i < fields.Length then
                        start_code fields.[frame.i]
                    else
                        accu <- frame.ary
                        stack_discard_top()
                | _ -> dontcare()
            | UEblockwith (orig_expr, idxl, exprl) ->
                let frame = frame :?> BlockwithFrame
                match frame.pc with
                | Tag.Start ->
                    frame.pc <- Tag.BlockWith_EvalOrig
                    start_code orig_expr
                | Tag.BlockWith_EvalOrig ->
                    frame.pc <- Tag.BlockWith_EvalSets
                    frame.tag <- (accu :?> MalBlock).Tag
                    frame.fields <- Array.copy ((accu :?> MalBlock).Fields)
                    start_code exprl.[0]
                | Tag.BlockWith_EvalSets ->
                    frame.fields.[idxl.[frame.i]] <- accu
                    frame.i <- frame.i + 1
                    if frame.i < idxl.Length then
                        start_code exprl.[frame.i]
                    else
                        accu <- blockCreate mm frame.tag frame.fields
                        stack_discard_top()
                | _ -> dontcare()
            | UEapply (expl, _) ->
                let frame = frame :?> ApplyFrame
                match frame.pc with
                | Tag.Start ->
                    frame.pc <- Tag.Apply_Eval
                    frame.values <- Array.zeroCreate<MalValue> expl.Length
                    start_code expl.[0]
                | Tag.Apply_Eval ->
                    frame.values.[frame.i] <- accu
                    frame.i <- frame.i + 1
                    if frame.i < expl.Length then
                        start_code expl.[frame.i]
                    else
                        frame.pc <- Tag.Apply_Apply
                        frame.i <- 0
                | Tag.Apply_Apply ->
                    // At this point, frame.values contains function and arguments.
                    // The number of arguments is >= 1.
                    // The function is at frame.values.[frame.i]. The function can be Vfunc, Vkfunc, Vclosure, Vpartial or Vcoroutine.
                    // The first argument is frame.values.[frame.i + 1].
                    // The last argument is frame.values.[frame.values.Length - 1].
                    let funcval = frame.values.[frame.i]
                    let arity =
                        match funcval.Kind with
                        | MalValueKind.CLOSURE -> (funcval :?> MalClosure).Arity
                        | MalValueKind.FUNC -> (funcval :?> MalFunc).Arity
                        | MalValueKind.KFUNC -> (funcval :?> MalKFunc).Arity
                        | MalValueKind.PARTIAL -> (funcval :?> MalPartial).Arity
                        | MalValueKind.COROUTINE -> (funcval :?> MalCoroutine).Arity
                        | _ -> dontcare()
                    let argc = frame.values.Length - frame.i - 1
                    if argc < arity then
                        accu <- MalPartial(arity - argc, Array.sub frame.values frame.i (frame.values.Length - frame.i))
                        stack_discard_top()
                    else
                        // if function is partial, expand it into frame.values.
                        if funcval.Kind = MalValueKind.PARTIAL then
                            let rec partial_length_loop (v : MalValue) =
                                match v.Kind with
                                | MalValueKind.PARTIAL ->
                                    let args = (v :?> MalPartial).Args
                                    partial_length_loop args.[0] + args.Length - 1
                                | MalValueKind.CLOSURE
                                | MalValueKind.FUNC
                                | MalValueKind.KFUNC
                                | MalValueKind.COROUTINE -> 1
                                | _ -> dontcare()
                            let partial_length = partial_length_loop frame.values.[frame.i]
                            let ary = Array.zeroCreate<MalValue> (partial_length + frame.values.Length - frame.i - 1)
                            let rec expand_loop ary (v : MalValue) =
                                match v.Kind with
                                | MalValueKind.PARTIAL ->
                                    let args = (v :?> MalPartial).Args
                                    let ofs = expand_loop ary args.[0]
                                    let n = args.Length - 1
                                    Array.blit args 1 ary ofs n
                                    ofs + n
                                | MalValueKind.CLOSURE
                                | MalValueKind.FUNC
                                | MalValueKind.KFUNC
                                | MalValueKind.COROUTINE ->
                                    ary.[0] <- v
                                    1
                                | _ -> dontcare()
                            let ofs = expand_loop ary frame.values.[frame.i]
                            Array.blit frame.values (frame.i + 1) ary ofs (frame.values.Length - frame.i - 1)
                            frame.values <- ary
                            frame.i <- 0
                        // call actual function
                        let funcval = frame.values.[frame.i]
                        match funcval.Kind with
                        | MalValueKind.FUNC ->
                            let func = funcval :?> MalFunc
                            let argv = Array.sub frame.values (frame.i + 1) func.Arity
                            try
                                let result = func.F mm argv
                                if frame.i + 1 + func.Arity = frame.values.Length then
                                    accu <- result
                                    stack_discard_top()
                                else
                                    let j = frame.i + func.Arity
                                    for k = frame.i to j - 1 do
                                        frame.values.[k] <- Unchecked.defaultof<MalValue>
                                    frame.i <- j
                                    frame.values.[j] <- result
                            with
                                | MalException v -> do_raise v
                                | MalUncatchableException msg ->
                                    state <- State.Failure
                                    send_message (Message.UncatchableException msg)
                        | MalValueKind.KFUNC ->
                            let kfunc = funcval :?> MalKFunc
                            let frame_values = Array.sub frame.values frame.i (frame.values.Length - frame.i)
                            try
                                let result = kfunc.F mm frame_values
                                if result.Length = 1 then
                                    accu <- result.[0]
                                    stack_discard_top()
                                else
                                    frame.values <- result
                                    frame.i <- 0
                            with
                                | MalException v -> do_raise v
                                | MalUncatchableException msg ->
                                    state <- State.Failure
                                    send_message (Message.UncatchableException msg)
                        | MalValueKind.COROUTINE ->
                            let vco = funcval :?> MalCoroutine
                            let arity = vco.Arity
                            let starter = vco.Starter
                            let argv = Array.sub frame.values (frame.i + 1) arity
                            let co = starter mm argv
                            let j = frame.i + 1 + arity
                            for k = frame.i to j - 1 do
                                frame.values.[k] <- Unchecked.defaultof<MalValue>
                            frame.i <- j
                            frame.pc <- Tag.Apply_Coroutine
                            co.Run mm.coroutine_initial_ticks
                            if co.IsFinished then
                                try
                                    let result = co.Result
                                    if frame.i = frame.values.Length then
                                        accu <- result
                                        stack_discard_top()
                                    else
                                        frame.i <- frame.i - 1
                                        frame.values.[frame.i] <- result
                                        frame.pc <- Tag.Apply_Apply
                                with
                                    | MalException v -> do_raise v
                                    | MalUncatchableException msg ->
                                        state <- State.Failure
                                        send_message (Message.UncatchableException msg)
                            else
                                stack_push UEcoroutine co
                        | MalValueKind.CLOSURE ->
                            let closure = funcval :?> MalClosure
                            let arity = closure.Arity
                            // Start evaluation of the closure
                            frame.oldenv <- env
                            frame.oldcaptures <- captures
                            env <- Array.zeroCreate<MalValue> closure.EnvSize
                            captures <- closure.Captures
                            for j = 0 to arity - 1 do
                                env.[j] <- frame.values.[frame.i + 1 + j]
                            let n = frame.i + 1 + arity
                            for m = frame.i to n - 1 do
                                frame.values.[m] <- Unchecked.defaultof<MalValue>
                            frame.pc <- Tag.Apply_Closure
                            frame.i <- n
                            frame.calling_code <- closure.Code
                            start_code closure.Code
                        | _ -> dontcare()
                | Tag.Apply_Closure ->
                    if frame.i = frame.values.Length then
                        stack_discard_top()
                    else
                        env <- frame.oldenv
                        captures <- frame.oldcaptures
                        frame.i <- frame.i - 1
                        frame.values.[frame.i] <- accu
                        frame.pc <- Tag.Apply_Apply
                | Tag.Apply_Coroutine ->
                    if frame.i = frame.values.Length then
                        stack_discard_top()
                    else
                        frame.i <- frame.i - 1
                        frame.values.[frame.i] <- accu
                        frame.pc <- Tag.Apply_Apply
                | _ -> dontcare()
            | UEtailcall args ->
                let frame = frame :?> TailcallFrame
                match frame.pc with
                | Tag.Start ->
                    frame.args <- Array.zeroCreate args.Length
                    frame.pc <- Tag.Tailcall_Eval
                    start_code args.[0]
                | Tag.Tailcall_Eval ->
                    frame.args.[frame.i] <- accu
                    frame.i <- frame.i + 1
                    if frame.i < args.Length then
                        start_code args.[frame.i]
                    else
                        let rec find_upper_apply_loop i =
                            match stack.[i].code with
                            | UEapply _ ->
                                let frame = stack.[i].frame :?> ApplyFrame
                                if frame.pc = Tag.Apply_Closure
                                then i
                                else find_upper_apply_loop (i - 1)
                            | _ -> find_upper_apply_loop (i - 1)
                        let upper_apply_idx = find_upper_apply_loop stack_topidx
                        while stack_topidx <> upper_apply_idx do
                            stack_discard_top()
                        for j = 0 to frame.args.Length - 1 do
                            env.[j] <- frame.args.[j]
                        start_code (stack.[stack_topidx].frame :?> ApplyFrame).calling_code
                | _ -> dontcare()
            | UEcoroutine ->
                let co = frame :?> IMalCoroutine
                let slice = max 0 (sliceTicks - (Environment.TickCount - tickCountAtStart))
                co.Run(slice)
                if co.IsFinished then
                    try
                        accu <- co.Result
                        stack_discard_top()
                    with
                    | MalException v ->
                        do_raise v
                    | MalUncatchableException msg ->
                        state <- State.Failure
                        send_message (Message.UncatchableException msg)
            | UEbegin cmdl ->
                let frame = frame :?> BeginFrame
                match frame.pc with
                | Tag.Start ->
                    if cmdl.Length = 0 then
                        accu <- unit
                        stack_discard_top()
                    else
                        frame.pc <- Tag.Begin_Eval
                        start_code cmdl.[0]
                | Tag.Begin_Eval ->
                    frame.i <- frame.i + 1
                    if frame.i < cmdl.Length then
                        start_code cmdl.[frame.i]
                    else stack_discard_top()
                | _ -> dontcare()
            | UEcase (test, cases) ->
                let frame = frame :?> CaseFrame
                match frame.pc with
                | Tag.Start ->
                    frame.pc <- Tag.Case_EvalTest
                    start_code test
                | Tag.Case_EvalTest ->
                    frame.testvalue <- accu
                    frame.pc <- Tag.Case_Match
                | Tag.Case_Match ->
                    let pat, when_clause, result = cases.[frame.i]
                    if do_match true pat frame.testvalue then
                        match when_clause with
                        | Some code ->
                            frame.pc <- Tag.Case_EvalWhen
                            start_code code // ew
                        | None ->
                            frame.pc <- Tag.Case_EvalResult
                            start_code result
                    else
                        frame.i <- frame.i + 1
                        if frame.i = cases.Length then
                            do_raise mal_Match_failure
                | Tag.Case_EvalWhen ->
                    let _, _, result = cases.[frame.i]
                    if toBool accu then
                        frame.pc <- Tag.Case_EvalResult
                        start_code result
                    else
                        frame.i <- frame.i + 1
                        if frame.i < cases.Length then
                            frame.pc <- Tag.Case_Match
                        else
                            do_raise mal_Match_failure
                | Tag.Case_EvalResult ->
                    stack_discard_top()
                | _ -> dontcare()
            | UEtry (expr_try, _) ->
                let frame = frame :?> ref<Tag>
                match !frame with
                | Tag.Start ->
                    frame := Tag.Try_EvalTry
                    start_code expr_try
                | Tag.Try_EvalTry
                | Tag.Try_EvalCatch ->
                    stack_discard_top()
                | _ -> dontcare()
            | UEif (code_test, code_true, code_false) ->
                let frame = frame :?> ref<Tag>
                match !frame with
                | Tag.Start ->
                    frame := Tag.If_EvalTest
                    start_code code_test
                |  Tag.If_EvalTest ->
                    frame := Tag.If_EvalResult
                    if toBool accu
                    then start_code code_true
                    else start_code code_false
                | Tag.If_EvalResult ->
                    stack_discard_top()
                | _ -> dontcare()
            | UEsetenvvar (ofs, code) ->
                let frame = frame :?> ref<Tag>
                match !frame with
                | Tag.Start ->
                    frame := Tag.Setenvvar_Eval
                    start_code code
                | Tag.Setenvvar_Eval ->
                    (env.[ofs] :?> MalVar).Content <- accu
                    stack_discard_top()
                | _ -> dontcare()
            | UEsetcapvar (ofs, code) ->
                let frame = frame :?> ref<Tag>
                match !frame with
                | Tag.Start ->
                    frame := Tag.Setcapturevar_Eval
                    start_code code
                | Tag.Setcapturevar_Eval ->
                    (captures.[ofs] :?> MalVar).Content <- accu
                    stack_discard_top()
                | _ -> dontcare()
            | UEfor (ofs, first, dir, last, body) ->
                let frame = frame :?> ForloopFrame
                match frame.pc with
                | Tag.Start ->
                    frame.pc <- Tag.Forloop_EvalFirst
                    start_code first
                | Tag.Forloop_EvalFirst ->
                    frame.i <- toInt accu
                    frame.pc <- Tag.Forloop_EvalLast
                    start_code last
                | Tag.Forloop_EvalLast ->
                    frame.j <- toInt accu
                    if dir = dirflag.Upto && frame.i <= frame.j then
                        frame.pc <- Tag.Forloop_To
                        env.[ofs] <- ofInt frame.i
                        start_code body
                    elif dir = dirflag.Downto && frame.i >= frame.j then
                        frame.pc <- Tag.Forloop_Downto
                        env.[ofs] <- ofInt frame.i
                        start_code body
                    else stack_discard_top()
                | Tag.Forloop_To ->
                    if frame.i < frame.j then
                        frame.i <- frame.i + 1
                        env.[ofs] <- ofInt frame.i
                        start_code body
                    else
                        accu <- unit
                        stack_discard_top()
                | Tag.Forloop_Downto ->
                    if frame.i > frame.j then
                        frame.i <- frame.i - 1
                        env.[ofs] <- ofInt frame.i
                        start_code body
                    else
                        accu <- unit
                        stack_discard_top()
                | _ -> dontcare()
            | UEwhile (cond, body) ->
                let frame = frame :?> ref<Tag>
                match !frame with
                | Tag.Start ->
                    frame := Tag.While_EvalCond
                    start_code cond
                | Tag.While_EvalCond ->
                    if toBool accu then
                        frame := Tag.While_EvalBody
                        start_code body
                    else
                        accu <- unit
                        stack_discard_top()
                | Tag.While_EvalBody ->
                    frame := Tag.While_EvalCond
                    start_code cond
                | _ -> dontcare()
            | UCval l ->
                let frame = frame :?> ValVarFrame
                match frame.pc with
                | Tag.Start ->
                    frame.pc <- Tag.Val_Eval
                    let _, code = l.[0]
                    start_code code
                | Tag.Val_Eval ->
                    let pat, _ = l.[frame.i]
                    if do_match true pat accu then
                        frame.i <- frame.i + 1
                        if frame.i < l.Length then
                            let _, code = l.[frame.i]
                            start_code code
                        else
                            stack_discard_top()
                    else
                        do_raise mal_Match_failure
                | _ -> dontcare()
            | UCvar defs ->
                let frame = frame :?> ValVarFrame
                match frame.pc with
                | Tag.Start ->
                    frame.pc <- Tag.Var_Eval
                    start_code (snd defs.[0])
                | Tag.Var_Eval ->
                    env.[fst defs.[frame.i]] <- MalVar(accu) :> MalValue
                    frame.i <- frame.i + 1
                    if frame.i < defs.Length then
                        start_code (snd defs.[frame.i])
                    else stack_discard_top()
                | _ -> dontcare()
            | UTCexpr (code, tyenv', alloc', ty) ->
                let frame = frame :?> ref<Tag>
                match !frame with
                | Tag.Start ->
                    frame := Tag.TopExpr_Eval
                    start_code code
                | Tag.TopExpr_Eval ->
                    tyenv <- tyenv'
                    alloc <- alloc'
                    send_message (Message.EvaluationComplete (tyenv, accu, ty))
                    stack_discard_top()
                | _ -> dontcare()
            | UTCvalvarfun (code, tyenv', alloc', shadowed, new_values) ->
                let frame = frame :?> ref<Tag>
                match !frame with
                | Tag.Start ->
                    frame := Tag.TopValVarFun_Eval
                    start_code code
                | Tag.TopValVarFun_Eval ->
                    tyenv <- tyenv'
                    alloc <- alloc'
                    for ofs in shadowed do
                        env.[ofs] <- Value.zero
                    print_new_values new_values
                    stack_discard_top()
                | _ -> dontcare()
            | _ -> dontcare()

            if state = State.Running then
                if stack_topidx = -1 then
                    state <- State.Success
                elif stack_topidx + 1 >= mm.maximum_stack_depth then
                    state <- State.Failure
                    send_message (Message.UncatchableException "Stack overflow")
                elif not (checkFreeMemory mm 0) then
                    state <- State.Failure
                    send_message (Message.UncatchableException "Insufficient memory")

            cycles <- cycles + 1L

    let alias new_name orig_name =
        clear()
        let vi = Option.get (Types.try_get_value tyenv orig_name)
        tyenv <- Types.add_value tyenv new_name vi
        let { ofs = orig_ofs } = alloc.Get(orig_name)
        let new_ofs = alloc.Add(new_name, Immutable)
        env <- array_ensure_capacity_exn mm.maximum_array_length (new_ofs + 1) env
        env.[new_ofs] <- env.[orig_ofs]
    
    let obj_of_value_cache = Dictionary<Type, HashSet<MalValue> -> MalValue -> obj>()
    let value_of_obj_cache = Dictionary<Type, MemoryManager -> obj -> MalValue>()
    
    let stacktrace limit (sb : StringBuilder) =
        let pfn fmt = Printf.kprintf (fun s -> sb.AppendLine s |> ignore) fmt
        let pfni fmt = Printf.kprintf (fun s ->
            sb.Append("  ") |> ignore
            sb.AppendLine s |> ignore) fmt
        let printLoc (loc : location) =
            let bgn = loc.st.AbsoluteOffset
            let ed = loc.ed.AbsoluteOffset
            let chunk =
                let sb = StringBuilder()
                let mutable pos = bgn
                sb.Add("\"")
                while pos < ed && pos < bgn + 50 do
                    sb.Add(Printer.escapedChar loc.src.[pos])
                    pos <- pos + 1
                sb.Add("\"")
                if pos <> ed then
                    sb.Add("...")
                sb.ToString()
            sprintf "%s (%d,%d) %s" loc.st.FileName (loc.st.Line+1) (loc.st.Column+1) chunk
        pfn "Stacktrace:"
        let st = min (limit - 1) stack_topidx
        if st <> stack_topidx then
            pfn "  (printing bottom %d frames only)" limit
        for i = st downto 0 do
            match stack.[i].code with
            | UEsetenvvar _ -> pfni "Setvar"
            | UEsetcapvar _ -> pfni "Setcapturevar"
            | UEblock _ -> pfni "Block"
            | UEblockwith _ -> pfni "Blockwith"
            | UEarray _ -> pfni "Array"
            | UEapply (_, loc) -> pfni "Apply %s" (printLoc loc)
            | UEtailcall _ -> pfni "UEtailcall"
            | UEcoroutine -> pfni "Coroutine"
            | UEbegin _ -> pfni "Begin"
            | UEcase _ -> pfni "Case"
            | UEtry _ -> pfni "Try"
            | UEif _ -> pfni "If"
            | UEfor _ -> pfni "For"
            | UEwhile _ -> pfni "While"
            | UCval _ -> pfni "Val"
            | UCvar _ -> pfni "Var"
            | UTCexpr _ -> pfni "TopExpr"
            | UTCvalvarfun _ -> pfni "TopValVarFun"
            | _ -> dontcare()

    member this.State with get() = state and set x = state <- x
    member this.Run(sliceTicks : int) = run sliceTicks
    member this.Accu = accu
    member this.Tyenv = tyenv
    member this.Alloc = alloc
    member this.Cycles = cycles
    member this.LatestMessage = latestMessage
    member this.MessageHook with get() = message_hook and set f = message_hook <- f
    member this.Runtime = mm
    member this.Store with get() = store and set o = store <- o
    member this.Result =
        match latestMessage with
        | Some (Message.EvaluationComplete (tyenv, ty, value)) -> (tyenv, ty, value)
        | _ -> raise (InvalidOperationException())
    member this.Clear() = clear()
    
    member this.Alias newName origName =
        alias newName origName
    
    member this.GEnv =
        if stack_topidx <> -1 then dontcare()
        env

    member this.Start(s) = start s

    member this.StartApply(values : MalValue array) =
        clear()
        let code = UEapply (Array.map (fun v -> UEconst v) values, dummyLoc "StartApply")
        start_code code
        state <- State.Running

    member this.Do(s) =
        start s
        run Int32.MaxValue

    member this.Set(name : string, value : MalValue, ty : type_expr) =
        clear()
        set name value ty Immutable |> ignore

    member this.Val<'T>(name : string, x : 'T) =
        clear()
        let v = Value.value_of_obj value_of_obj_cache tyenv typeof<'T> Value.dummy_mm x
        let ty = Types.typeexpr_of_type tyenv (Dictionary()) [] typeof<'T>
        set name v ty Immutable |> ignore

    member this.Var<'T>(name : string, x : 'T) =
        clear()
        let v = Value.value_of_obj value_of_obj_cache tyenv typeof<'T> Value.dummy_mm x
        let ty = Types.typeexpr_of_type tyenv (Dictionary()) [] typeof<'T>
        set name v ty Mutable

    member this.GetVar(name : string) : MalVar =
        clear()
        let { ofs = ofs; access = access } = alloc.Get(name)
        match access with
        | Immutable -> dontcare()
        | Mutable -> env.[ofs] :?> MalVar

    member this.GetValue(name : string) : MalValue =
        clear()
        let { ofs = ofs; access = access } = alloc.Get(name)
        match access with
        | Immutable -> env.[ofs]
        | Mutable -> (env.[ofs] :?> MalVar).Content

    member this.Get<'T>(name : string) =
        let value = this.GetValue(name)
        let obj = Value.obj_of_value obj_of_value_cache tyenv (touch_create()) typeof<'T> value
        obj :?> 'T
    
    member this.GetFromOfs<'T>(ofs : int) =
        clear()
        let value = env.[ofs]
        let value = if value.Kind = MalValueKind.VAR then (value :?> MalVar).Content else value
        let obj = Value.obj_of_value obj_of_value_cache tyenv (touch_create()) typeof<'T> value
        obj :?> 'T

    member this.Fun<'T, 'U>(name : string, arg_names : string list, f : MemoryManager -> 'T -> 'U) =
        clear()
        let v = Value.wrap_fsharp_func tyenv obj_of_value_cache value_of_obj_cache typeof<MemoryManager -> 'T -> 'U> f
        let ty = Types.typeexpr_of_type tyenv (Dictionary()) arg_names typeof<'T -> 'U>
        set name v ty Immutable |> ignore

    member this.Fun<'T, 'U>(name : string, f : MemoryManager -> 'T -> 'U) =
        this.Fun<'T, 'U>(name, [], f)

    member this.RegisterAbstractType(name : string, ty : Type) =
        clear()
        let tyenv', id = Types.register_abstract_type tyenv name ty
        tyenv <- tyenv'

    member this.RegisterFsharpTypes(types : (string * Type) array) =
        clear()
        let tyenv', id = Types.register_fsharp_types tyenv types
        tyenv <- tyenv'

    member this.Stacktrace(limit, sb) = stacktrace limit sb

    member this.ObjOfValue<'T>(value : MalValue) = Value.obj_of_value obj_of_value_cache tyenv (touch_create()) typeof<'T> value :?> 'T
    member this.ValueOfObj<'T>(obj : 'T) = Value.value_of_obj value_of_obj_cache tyenv typeof<'T> Value.dummy_mm obj
    member this.Typeof<'T>(argNames) = Types.typeexpr_of_type tyenv (Dictionary()) argNames typeof<'T>
    member this.Typeof<'T>() = Types.typeexpr_of_type tyenv (Dictionary()) [] typeof<'T>
