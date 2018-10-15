module Malmacs.Program
open System
open System.IO
open System.Diagnostics
open System.Windows.Forms
open System.Runtime.InteropServices
open System.Reflection

[<DllImport("user32.dll")>]
extern bool SetProcessDPIAware()

[<EntryPoint; STAThread>]
let main args = 
    if Environment.OSVersion.Version.Major >= 6 then SetProcessDPIAware() |> ignore
    Application.EnableVisualStyles()
    Application.SetCompatibleTextRenderingDefault(false)
    Application.SetUnhandledExceptionMode(UnhandledExceptionMode.ThrowException)

    let repl = new Repl()

    AppDomain.CurrentDomain.UnhandledException.Add(fun ev ->
        let logDir = Environment.GetFolderPath(Environment.SpecialFolder.Desktop)
        let id = sprintf "Malmacs-%s-%d-" (DateTime.Now.ToString("yyyyMMddHHmmss")) (Process.GetCurrentProcess().Id)
        let exn = ev.ExceptionObject
        File.WriteAllText(Path.Combine(logDir, id + "Crash-Exn.txt"), exn.ToString())
        for i = 0 to repl.Editors.Count - 1 do
            let editor = repl.Editors.[i]
            let name =
                match editor.TextFileHandle with
                | None -> sprintf "Untitled%d.txt" (i + 1)
                | Some h -> Path.GetFileName (h.OriginalPath)
            let text = Doc.getAllString editor.Doc
            File.WriteAllText(Path.Combine(logDir, id + name), text))

    Application.Idle.Add(fun ev ->
        if repl.Editors.Count = 0 then
            Application.Exit()
        let mutable m = Win32.Message()
        while (not (Win32.PeekMessage(&m, IntPtr.Zero, 0u, 0u, 0u))) && repl.Tick() do ())

    if args.Length > 0 then
        repl.OpenPathWithNewEditor(args.[0])
    else
        repl.NewEditor()

    Application.Run()

    0