﻿open System
open Printf

open FsMiniMAL
open FsMiniMAL.Value
open System.Numerics

type foobar<'a> = Foo | Bar of hogefuga<'a>
and hogefuga<'a> = Hoge of 'a | Fuga of foobar<'a>

[<EntryPoint>]
let main argv = 

    let topcase source answer =
        let mal = Top.createInterpreter()
        mal.Do (source)
        match mal.State with
        | State.Success ->
            let tyenv, value, ty = mal.Result
            let result = Printer.print_value_without_type tyenv 10000 ty value
            if result = answer then
                ()
            else
                printfn "Test failed."
                printfn "Source: %s" source
                printfn "Expected %s but %s." answer result
        | State.Failure ->
            printfn "Test failed (mal error)."
            printfn "Source: %s" source
        | State.Running ->
            printfn "Test failed (interpreter crashed)."
            printfn "Source: %s" source
        | _ -> failwith ""

    let case source answer =
        topcase source answer
        topcase (String.Concat("begin ", source, " end")) answer
    
    let type_error source = 
        let mal = Top.createInterpreter()
        try mal.Do (source) with _ -> ()
        if mal.State = State.Failure && (match mal.LatestMessage with Some (Message.TypeError _) -> true | _ -> false) then
            ()
        else
            printfn "Test failed. (expected type error)"
            printfn "Source: %s" source

    let insufficient_memory source =
        let mm =
            { memory_manager_create_default() with
                  bytes_trigger_gc = 20_000
                  bytes_stop_exec = 10_000 }
        let mal = Top.createInterpreterExt(mm)
        try
            mal.Do (source)

            let failed() =
                printfn "Test failed."
                printfn "Source: %s" source
            
            if mal.State = State.Failure then
                match mal.LatestMessage with
                | Some (Message.UncatchableException "Insufficient memory") -> ()
                | _ -> failed()
            else failed()
        with exn ->
            printfn "Test failed. Should fail safely but raises exception."
            printfn "Source: %s" source
            printfn "exn: %s %s" (exn.GetType().Name) exn.Message
    
    let timeout slice source =
        let mal = Top.createInterpreter()
        mal.Start(source)
        mal.Run(slice)
        if mal.State = State.Running then ()
        else
            printfn "Test failed. Should timeout but didn't."
            printfn "Source: %s" source

    let keep_shadowed_globals_after_failure () =
        let mal = Top.createInterpreter() 
        mal.Do("val n = 100")
        mal.Do("val n = 200 / 0")
        mal.Do("n")

        let tyenv, value, ty = mal.Result
        if toInt value = 100 then
            ()
        else
            printfn "Test keep_shadowed_globals_after_failure failed."

    let apply_side_effects_on_tyenv_even_if_execution_failed () =
        let mal = Top.createInterpreter() 
        mal.Do("val a = [||]")
        mal.Do("begin arrayAdd a 100; 1 / 0 end")
        mal.Do("a.[0]")

        let tyenv, value, ty = mal.Result
        if Unify.same_type tyenv ty Types.ty_int && toInt value = 100 then
            ()
        else
            printfn "Test apply_side_effects_on_tyenv_even_if_execution_failed failed."
    
    let int_min = "-2147483648"

    let sw = System.Diagnostics.Stopwatch.StartNew()

    case "()" "()"
    case "true" "true"
    case "false" "false"
    case "1" "1"
    case "-1" "-1"
    case int_min int_min
    case "1.0" "1.0"
    case "nan" "nan"
    
    case "Some 0" "Some 0"
    case "Some (-1)" "Some (-1)"
    case "Some (1, 1)" "Some (1, 1)"
    case "Some (1, -1)" "Some (1, -1)"
    case "Some (-1, 1)" "Some (-1, 1)"
    case "Some (-1, -1)" "Some (-1, -1)"

    case "1 + 1" "2"
    case "1.0 / 3.0" "0.33333333333333331"

    case "1 = 1" "true"
    case "1 = 2" "false"
    
    case "1 <> 1" "false"
    case "1 <> 2" "true"

    case "1 < 2" "true"
    case "1 < 1" "false"
    case "2 < 1" "false"

    case "1 > 2" "false"
    case "1 > 1" "false"
    case "2 > 1" "true"

    case "1 <= 2" "true"
    case "1 <= 1" "true"
    case "2 <= 1" "false"

    case "1 >= 2" "false"
    case "1 >= 1" "true"
    case "2 >= 1" "true"

    case "nan = nan" "false"
    case "nan <> nan" "true"
    case "nan = 0.0" "false"
    case "nan <> 0.0" "true"

    case "(); 1" "1"
    case "val x = 1; x + x" "2"
    case "var x = 0; x <- x + 1; x " "1"
    case "fun f x = x + 1; f 0" "1"
    case "val x = 1; fun f y = x + y; val x = 2; f 3" "4"
    case "var n = 0; n <- n + 1; n <- n + 1; n" "2"
    case "fun f n = n + n; fun g n = f n + n; g 10" "30"

    case "fun fib n = if n <= 2 then 1 else fib (n-1) + fib (n-2); fib 20" "6765"

    case "1.0 + 0.5" "1.5"

    case "\"hello\"" "\"hello\""
    case "\"hello\" ^ \" world\"" "\"hello world\""

    case "var n = 0; for i = 1 to 1000 do n <- n + i; n" "500500"

    case "val a = [||]; for i = 2147483647 to 2147483647 do arrayAdd a i; a" "[|2147483647|]"
    case "val a = [||]; for i = 2147483646 to 2147483647 do arrayAdd a i; a;" "[|2147483646, 2147483647|]"
    case "val a = [||]; for i = 2147483645 to 2147483647 do arrayAdd a i; a;" "[|2147483645, 2147483646, 2147483647|]"

    case "val a = [||]; for i = 2147483647 downto 2147483647 do arrayAdd a i; a" "[|2147483647|]"
    case "val a = [||]; for i = 2147483647 downto 2147483646 do arrayAdd a i; a" "[|2147483647, 2147483646|]"
    case "val a = [||]; for i = 2147483647 downto 2147483645 do arrayAdd a i; a" "[|2147483647, 2147483646, 2147483645|]"

    case "val a = [||]; for i = 2147483647 to 2147483646 do arrayAdd a i; a" "[||]"
    case "val a = [||]; for i = 2147483646 downto 2147483647 do arrayAdd a i; a" "[||]"

    case "val a = [||]; for i = -2147483648 to -2147483648 do arrayAdd a i; a" "[|-2147483648|]"
    case "val a = [||]; for i = -2147483648 to -2147483647 do arrayAdd a i; a" "[|-2147483648, -2147483647|]"
    case "val a = [||]; for i = -2147483648 to -2147483646 do arrayAdd a i; a" "[|-2147483648, -2147483647, -2147483646|]"

    case "val a = [||]; for i = -2147483648 downto -2147483648 do arrayAdd a i; a" "[|-2147483648|]"
    case "val a = [||]; for i = -2147483647 downto -2147483648 do arrayAdd a i; a" "[|-2147483647, -2147483648|]"
    case "val a = [||]; for i = -2147483646 downto -2147483648 do arrayAdd a i; a" "[|-2147483646, -2147483647, -2147483648|]"

    case "val a = [||]; for i = -2147483647 to -2147483648 do arrayAdd a i; a" "[||]"
    case "val a = [||]; for i = -2147483648 downto -2147483647 do arrayAdd a i; a" "[||]"

    case "var sum = 0; var i = 0; while i <= 100 do begin sum <- sum + i; i <- i + 1 end; sum" "5050"

    case "[|1,2,3,4,5|]" "[|1, 2, 3, 4, 5|]"
    case "[1,2,3]" "[1, 2, 3]"
    case "[[[[]]]]" "[[[[]]]]"
    case "[|[|[|[||]|]|]|]" "[|[|[|[||]|]|]|]"

    case "fun f a b c d e f g h i j k l m n = a + b + c + d + e + f + g + h + i + j + k + l + m + n; f 1 2 3 4 5 6 7 8 9 10 11 12 13 14" "105"
    case "fun f a b c d e f g h i j k l m n = a + b + c + d + e + f + g + h + i + j + k + l + m + n; val g = f 1 2 3 4 5; val h = g 6 7 8 9 10; h 11 12 13 14" "105"
    case "(fn a -> (fn b -> (fn c -> (fn d -> [|a, b, c, d|])))) 1 2 3 4" "[|1, 2, 3, 4|]"

    case "arrayGet [| (fn a -> (fn b -> [a, b])) |] 0 1 2" "[1, 2]" // apply builtin which returns closure at once
    case "(fn a -> ( + )) 0 1 2" "3" // apply closure which returns builtin at once

    case """kprintf (fn s -> ( + )) "hello %f" 1.0 3 7""" "10"
    case "val x = 1; val f = begin val y = 2; begin fun f z = x + y + z; f end end; f 3" "6"
    case "val t = (1, 2); val (y, x) = t; (x, y)" "(2, 1)"
    case " case 1 + 1 of 1 -> false | 2 -> true | n -> false" "true"

    case "case 3 of 1 -> 1 | 2 -> 4 | n -> n * n" "9"
    case "case true || false of true -> 1 | false -> 0" "1"
    case "case [| 1, 2, 3 |] of [| i, j |] -> 0 | [| i, j, k |] -> i + j + k" "6"
    case """case "bar" of "foo" -> 1  | "bar" -> 2 | "baz" -> 3""" "2"

    case "false && (1 / 0) = 0" "false"
    case "true || (1 / 0) = 0" "true"
    case "(( || ) false) true" "true"
    case "(( && ) true) false" "false"

    // short-circuit tests
    case "var x = false; var y = false; (begin x <- true; false end) && (begin y <- true; false end); (x, y)" "(true, false)"
    case "var x = false; var y = false; (begin x <- true; true  end) || (begin y <- true; true  end); (x, y)" "(true, false)"
    case "var x = false; var y = false; ( && ) (begin x <- true; false end) (begin y <- true; false end); (x, y)" "(true, false)"
    case "var x = false; var y = false; ( || ) (begin x <- true; true  end) (begin y <- true;  true end); (x, y)" "(true, false)"
    case "var x = false; var y = false; val f = ( && ); f (begin x <- true; false end) (begin y <- true; false end); (x, y)" "(true, true)"
    case "var x = false; var y = false; val f = ( || ); f (begin x <- true; true  end) (begin y <- true; true  end); (x, y)" "(true, true)"

    case "begin begin begin 123 end end end" "123"

    case "[|0|].[0]" "0"
    case "1+1" "2"
    case "3-2" "1"
    case "7*4" "28"
    case "15/3" "5"
    case "27 % 6" "3"
    case "-3" "-3"
    case "10 ||| 1 ||| 4" "15"
    case "11 ^^^ 8" "3"
    case "10 &&& 3" "2"
    case "3.0 + 2.5" "5.5"
    case "2.0 ** 3.0" "8.0"
    case "1e10" "10000000000.0"
    case "1e-5" "1e-05"
    case "-10.0 * - 7.0" "70.0"
    case "1 = 1" "true"
    case "2 <= 2" "true"
    case "2 < 2" "false"
    case "true && true" "true"
    case "true && false" "false"
    case "not false" "true"
    case "[1,2,3] @ [4,5,6]" "[1, 2, 3, 4, 5, 6]"
    case """[| "foo", "bar", "baz" |].[1]""" "\"bar\""

    case "val a = [| false, false, false |]; a.[2] <- true; a" "[|false, false, true|]"

    topcase "type foobar = Foo | Bar; [|Foo, Bar|]" "[|Foo, Bar|]"

    topcase "
    type foobar = Foo | Bar of int;
    val a = [| Foo, Foo, Bar 1, Bar 2 |];
    fun f x = case x of
        | Foo -> 1
        | Bar n -> n * 100;
    var sum = 0;
    for i = 0 to arrayLength a - 1 do
        sum <- sum + f a.[i];
    sum" "302"

    case "begin val n = 1; val n = n + n; val n = n + n; n end;" "4"

    case "compare (-infinity) nan" "1"
    case "compare [| 0, 0 |] [|  1 |]" "1"
    case "compare [| 0, 0 |] [| -1 |]" "1"
    case "compare [| nan, 1.0 |] [| nan, 1.0 |]" "0"
    case "compare [| nan, 2.0 |] [| nan, 1.0 |]" "1"
    case "compare [| -infinity, 0.0 |] [| nan, 1.0 |]" "1"
    case "compare [| nan, 0.0 |] [| nan, 1.0 |]" "-1"
    topcase "type foobar = Foo | Bar of int; compare (Bar 1) Foo" "1"

    case "var n = 100; n <- n + 23; n" "123"

    type_error "case 3 of Foo -> 3"

    case """case (1, 2) of (a, b) when a = b -> "=" | (i, j) when i < j -> "<" | _ -> "other" """ "\"<\""

    type_error "case (1.0, 2.0) of (a, b) when a = b  -> () | (x, y) when a < b -> ()"

    insufficient_memory "val a = [||]; while true do arrayAdd a 1.0"

    topcase "type foobar = Foo | Bar of int; val l = [ Foo, Bar 100 ]; hide foobar; l" "[<abstr>, <abstr>]"

    topcase "type 'a mylist = Cons of ('a * 'a mylist) | Nil; Cons (1, Cons (2, Cons (3, Nil)))" "Cons (1, Cons (2, Cons (3, Nil)))"

    type_error "type foo == foo"
    type_error "type foo == foo list"
    type_error "type foo == bar and bar == foo"
    type_error "type foo == bar and bar == foo list"
    type_error "type 'a foo == int foo"
    type_error "type 'a foo == foo list"

    case "val i = 0; case (1, 2) of (i, 3) -> 4 | _ -> i" "0"

    case """val x = 1; try failwith "error" catch Efailure "error" -> x + 100""" "101"
    case """try failwith "Error" catch Efailure msg -> msg""" "\"Error\""
    topcase "exception FooException of int * int; try raise (FooException (1,2)) catch Efailure _ -> 0 | FooException (n, m) -> n + m" "3"

    case "val a = 1 and b = 2; a + b" "3"
    case "var a = 1 and b = 2; a + b" "3"

    type_error "type myint == int; (0.0 : myint)"

    topcase "type rec1 = { rec1_field : int ref }; val x = { rec1_field = ref 0 }; !x.rec1_field" "0" // syntax check. dot vs deref.
    
    keep_shadowed_globals_after_failure ()
    apply_side_effects_on_tyenv_even_if_execution_failed ()

    insufficient_memory "arrayCreate (1000 * 1000* 1000) 0"
    insufficient_memory "arrayCreate (1000 * 1000* 1000) 0.0"

    case "\"foo\" ^ \"bar\"" "\"foobar\""

    case "[||] ^ [||]" "[||]"
    case "[|1,2,3|] ^ [||]" "[|1, 2, 3|]"
    case "[||] ^ [|4,5,6|]" "[|4, 5, 6|]"
    case "[|1,2,3|] ^ [|4,5,6|]" "[|1, 2, 3, 4, 5, 6|]"

    case "[||] ^ [||]" "[||]"
    case "[|1.0,2.0,3.0|] ^ [||]" "[|1.0, 2.0, 3.0|]"
    case "[||] ^ [|4.0,5.0,6.0|]" "[|4.0, 5.0, 6.0|]"
    case "[|1.0,2.0,3.0|] ^ [|4.0,5.0,6.0|]" "[|1.0, 2.0, 3.0, 4.0, 5.0, 6.0|]"

    case """ "foo" ^ "bar" """ "\"foobar\""
    case """ "foo" ^^ "bar" """ "\"foobar\""

    case "kprintf (fn s -> 3); ()" "()"

    topcase "fun f x = (x : 'a); f 0; f 0.0" "0.0"
    type_error "begin fun f x = (x : 'a); f 0; f 0.0 end"

    case "begin end" "()"

    case "(( + ) 1.0) 1.0" "2.0"

    case "fun f x = 2.0 * x * x; f 3.0" "18.0"

    topcase "type foo = { mutable foo : foo option }; val x = {foo = None}; x.foo <- Some x; x" "{foo = Some ...}"
        
    timeout 100 "while true do ()"
    timeout 100 "var n = 0; while true do n <- n + 1"

    topcase "type foo1 = { foo : int }; type foo2 = { foo : int }; ({ foo = 0 } : foo1); ()" "()" // Accept this with no type error.
    topcase "type foo1 = Foo of int; type foo2 = Foo of int; ((Foo 0) : foo1); ()" "()" // Accept this with no type error.

    topcase "type foo1 = { foo : int }; type foo2 = { foo : string }; (fn (x : foo1) -> x.foo + 1); ()" "()" // Accept this with no type error.
    topcase "type foo1 = Foo of int; type foo2 = Foo of string; (fn x -> case (x : foo1) of Foo x -> x); ()" "()" // Accept this with no type error.

    topcase "type foo1 = { foo : int }; type foo2 = { foo : int }; (fn ({ foo = n } : foo1) -> n); ()" "()"
    type_error "fun p i = 10e-3 * i * i; p 0"

    case "fun f (x : float) = x + x; f 1.0" "2.0"

    let fsharp_interop () =
        try
            let mal = Top.createInterpreter()
            mal.RegisterFsharpTypes([|("foobar", typedefof<foobar<_>>); ("hogefuga", typedefof<hogefuga<_>>)|])
            mal.Fun("foobar_roundtrip", (fun mm (x : foobar<int>) -> x))
            mal.Fun("intlist_roundtrip", (fun mm (x : int list) -> x))
            let getResult() =
                try
                    let tyenv, value, ty = mal.Result
                    Some (Printer.print_value_without_type tyenv 10000 ty value)
                with _ -> None

            let cases =
                [| ("foobar_roundtrip Foo", "Foo")
                   ("foobar_roundtrip (Bar (Hoge 0))", "Bar (Hoge 0)")
                   ("foobar_roundtrip (Bar (Fuga Foo))", "Bar (Fuga Foo)")
                   ("intlist_roundtrip [1, 2, 3]", "[1, 2, 3]")|]

            for (src, result) in cases do
                mal.Do(src)
                match getResult() with
                | None -> failwith (src + " => " + result)
                | Some s -> if s <> result then failwith (src + " => " + result)
        with Failure msg -> printfn "Error(fsharp_interop): %s" msg
    
    fsharp_interop()

    topcase
        "lex { fun token =
  ['a' - 'z']+ { Some (lexbuf.lbStartPos, lexbuf.lbEndPos) }
  _            { token lexbuf }
  eof          { None }};
val lb = lexbufCreate \"hello world\";
val accu = [||];
while not lb.lbEof do
    accu << token lb;
accu"
        "[|Some (0, 5), Some (6, 11), None|]"
    
    let tailcall_test() =
        let interp = Top.createInterpreterExt({ memory_manager_create_default() with maximum_stack_depth = 100 })
        interp.Do("fun loop accu n m = if n <= m then loop (accu + n) (n + 1) m else accu")
        interp.Do("loop 0 1 1000")
        if not (interp.State = State.Success && toInt interp.Accu = 500500) then
            printfn "Tailcall test failed."
    tailcall_test()

    topcase "type xyz = { x : int, y : float, z : int }; val foo = { x = 1, y = 2.0, z = 3 }; { foo with z = 5 }" "{x = 1, y = 2.0, z = 5}"

    topcase "type 'a foo == int; [(0 : string foo), (1 : float foo)]; ()" "()" // The types of two values are looks different, but same after expanding abbreviation.

    printfn "Done."
    //printfn "Elapsed: %d (ms)" sw.ElapsedMilliseconds
    printfn "Press any key to close."
    Console.ReadKey() |> ignore
    0
