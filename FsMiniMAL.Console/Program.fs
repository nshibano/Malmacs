open System
open System.Diagnostics
open System.Threading
open System.IO
open System.Text

open FsMiniMAL
open FsMiniMAL.Types
open FsMiniMAL.Value

[<EntryPoint>]
let main argv =

    let lang =
        match System.Globalization.CultureInfo.CurrentCulture.Name with
        | "ja-JP" -> Printer.Ja
        | _ -> Printer.En

    let history =
        let path = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments), "mal_history.txt")
        try
            let f =new StreamWriter(path, true)
            f.WriteLine("// " + DateTime.Now.ToString())
            f.Flush()
            printfn "Command history is saved to %s" path
            Some f
        with _ ->
            printfn "!!! Failed to open history file. !!!"
            None
        
    let mal = Top.createInterpreter()
    mal.Fun("print_string", (fun mm (s : string) -> Console.Write(s)))
    mal.Do("""fun printf fmt = kprintf (fn s -> print_string s) fmt""")
    mal.Do("""fun printfn fmt = kprintf (fn s -> (print_string s; print_string "\r\n")) fmt""")
    mal.Store <- DateTime.MinValue
    mal.Fun("sleep", (fun mm t ->
        let wakeup = DateTime.Now.AddSeconds(t)
        mal.Store <- wakeup
        mal.State <- State.Paused))
    mal.Set("print_uvalue", MalFunc (1, (fun mm argv ->
        let s = 
            if box argv.[0] = null
            then "<null>\r\n"
            else Printf.sprintf "%A" argv.[0]
        Console.WriteLine(s)
        Value.unit)), arrow ty_a ty_unit)
    mal.MessageHook <- (fun msg -> Console.Write(FsMiniMAL.Printer.print_message lang (Console.WindowWidth - 1) msg))
    
    Console.WriteLine("> FsMiniMAL")
    Console.WriteLine("")

    let lock l f =
        Monitor.Enter(l)
        let result = f()
        Monitor.Exit(l)
        result

    Console.CancelKeyPress.Add(fun ev ->
            lock mal (fun () -> mal.Clear())
            ev.Cancel <- true)

    while true do
        Console.Write("# ")
        let s = Console.ReadLine()
        Option.iter (fun (f : StreamWriter) ->
            f.WriteLine(s)
            f.Flush()) history

        if isNull s then
            exit 0
        else
            lock mal (fun () -> mal.Start(s))

        while
            lock mal (fun () ->
                match mal.State with
                | State.Running ->
                    mal.Run(100)
                    true
                | State.Paused ->
                    if (mal.Store :?> DateTime) > DateTime.Now then
                        Thread.Sleep(100)
                        true
                    else
                        mal.Run(100)
                        true
                | State.Success
                | State.Clear ->
                    false
                | State.Failure ->
                    match mal.LatestMessage with
                    | Some (Message.UncaughtException (tyenv, exn)) ->
                        let sb = StringBuilder()
                        mal.Stacktrace(10, sb)
                        Console.Write(sb.ToString())
                    | _ -> ()
                    false
                | _ -> dontcare()) do ()
    0