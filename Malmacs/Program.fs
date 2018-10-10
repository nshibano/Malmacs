module Malmacs.Program
open System
open System.IO
open System.Diagnostics
open System.Windows.Forms
open System.Runtime.InteropServices
open System.Reflection

[<DllImport("user32.dll")>]
extern bool SetProcessDPIAware()

let readJson path name =
    try
        let s = System.IO.File.ReadAllText(path)
        MalJson.json.Parse(s)
    with exn ->
        Debug.WriteLine(sprintf "%s json read failed: %s" name exn.Message)
        MalJson.json.Jobject [||]

[<EntryPoint; STAThread>]
let main args = 
    if Environment.OSVersion.Version.Major >= 6 then SetProcessDPIAware() |> ignore
    Application.EnableVisualStyles()
    Application.SetCompatibleTextRenderingDefault(false)
    
    let repl = new Repl()
    MalLibs.add repl

    let exeDir = Path.GetDirectoryName(Assembly.GetEntryAssembly().Location)

    let configJsonPath = Path.Combine(exeDir, "config.json")
    let configJson = readJson "Config" configJsonPath

    let persistentJsonPath = Path.Combine(exeDir, "persistent.json")
    let persistentJson = readJson "Persistent" persistentJsonPath
    let persistentJsonOfs = repl.Mal.Var("persistent_json", persistentJson)

    Application.Idle.Add(fun ev ->
        if repl.Editors.Count = 0 then
            Application.Exit()
        let mutable m = Win32.Message()
        while (not (Win32.PeekMessage(&m, IntPtr.Zero, 0u, 0u, 0u))) && repl.Tick() do ())

    let initMalPath = Path.Combine(exeDir, "init.mal")
    try
        let src = File.ReadAllText(initMalPath)
        repl.Run(src)
        let start = Environment.TickCount
        while repl.Tick() && Environment.TickCount - start < 1000 do () // run 1000ms to hopefully finish init.mal and get malproc ready.
    with _ -> ()

    if args.Length > 0 then
        repl.OpenPathWithNewEditor(args.[0])
    else
        repl.NewEditor()

    Application.Run()

    let json =
        try repl.Mal.GetFromOfs(persistentJsonOfs)
        with _ -> MalJson.json.Jobject [||]
    let json =
        match json with
        | MalJson.json.Jobject _
        | MalJson.json.Jarray _ -> json
        | _ -> MalJson.json.Jobject [|("root_atom_backup", json)|]
    try
        File.WriteAllText(persistentJsonPath, json.ToString(false))
    with exn ->
        Debug.WriteLine(sprintf "Persistent json write failed: %s" exn.Message)
    0