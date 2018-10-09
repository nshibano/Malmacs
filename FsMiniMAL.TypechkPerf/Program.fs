// Learn more about F# at http://fsharp.org

open System
open System.Diagnostics

let src = """
fun fib n =
    if n <= 1 then
        n
    else
        fib (n - 1) + fib (n - 2)

fun integ n =
  begin
    val dx = 1.0 / float n;
    fun f x = 3.0 * x * x;
    var accu = 0.0;
    accu <- accu + 0.5 * f 0.0;
    for i = 1 to n - 1 do
      begin
        val x = float i * dx;
        accu <- accu + f x;
      end;
    accu <- accu + 0.5 * f 1.0;
    accu * dx
  end;

fun vector_loop n =
  begin
    val dx = 1.0 / float (n - 1);
    val a = [||];
    for i = 0 to n - 1 do arrayAdd a 1.0;
    val b = [||];
    for i = 0 to n - 1 do arrayAdd b (float i * dx);
    val c = [||];
    for i = 0 to n - 1 do arrayAdd c (a.[i] + b.[i]);
    c
  end;

fun vector_lambda n =
  begin
    val dx = 1.0 / float (n - 1);
    val a = arrayInit n (fn _ -> 1.0);
    val b = arrayInit n (fn i -> float i * dx);
    val c = arrayInit n (fn i -> a.[i] + b.[i]);
    c
  end;

type 'a node =
  | Nil
  | Node of 'a node_fields

and 'a node_fields =
  { level : int,
    left : 'a node,
    value : 'a,
    right : 'a node };

fun skew node =
  case node of
  | Node { level = level, left = Node { level = left_level, left = a, value = x, right = b }, value = y, right = c } when level = left_level ->
      Node { level = level, left = a, value = x, right = skew (Node { level = level, left = b, value = y, right = c })}
  | _ -> node;

fun level_of node =
  case node of
  | Nil -> 0
  | Node { level = level } -> level;

fun split node =
  case node of
  | Node { level = level, left = a, value = x, right = Node {level = right_level, left = b, value = y, right = c}} when level = level_of c ->
      Node { level = level + 1, left = Node { level = level, left = a, value = x, right = b }, value = y, right = split c }
  | _ -> node;

fun balance node = split (skew node);

fun add node x =
  case node of
  | Nil -> Node { level = 1, left = Nil, value = x, right = Nil }
  | Node fields ->
    if x < fields.value then
      balance (Node { level = fields.level, left = add fields.left x, value = fields.value, right = fields.right })
    else if fields.value < x then
      balance (Node { level = fields.level, left = fields.left, value = fields.value, right = add fields.right x })
    else node;
 
fun aatree n =
  begin
    var tree = Nil;
    for i = 1 to n do
      tree <- add tree i;
    tree
  end;
"""

[<EntryPoint>]
let main argv =
    let interp = FsMiniMAL.Top.createInterpreter()
    let n = 100
    let sw = Stopwatch.StartNew()
    for i = 0 to n - 1 do
        interp.Start(src)
    sw.Stop()
    printfn "%d (us)" (int (1e6 * float sw.ElapsedTicks / float Stopwatch.Frequency / float n))
    Console.ReadKey() |> ignore
    0
