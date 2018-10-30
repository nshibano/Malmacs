open System

open FsMiniMAL.Value

[<EntryPoint>]
let main argv =

    printfn "Is64BitProcess : %b" Environment.Is64BitProcess

    let get_total_memory () =
        GC.Collect()
        GC.WaitForPendingFinalizers()
        GC.Collect()
        GC.GetTotalMemory(false)
        
    let many = 100_000

    let test constr =
        let ary = Array.zeroCreate<Value> many
        let x0 = get_total_memory()
        for i = 0 to ary.Length - 1 do
            ary.[i] <- constr dummy_mm i
        let x1 = get_total_memory()
        ary.[0] |> ignore // To keep ary in scope. In release build this changes result.
        float (x1 - x0) / float many
        
    let sizeof_int = test (fun mm i -> of_int mm i)
    printfn "sizeof int: %.1f" sizeof_int

    let sizeof_float = test (fun mm i -> of_float mm (float i))    
    printfn "sizeof float: %.1f" sizeof_float

    let sizeof_block_len0 = test (fun mm i -> block_create mm 0 (Array.create 0 unit))
    let sizeof_block_len1000 = test (fun mm i -> block_create mm 0 (Array.create 1000 unit))
    let block_overhead = sizeof_block_len0
    let block_increment = (sizeof_block_len1000 - sizeof_block_len0) / 1000.0
    printfn "block overhead: %.1f" block_overhead
    printfn "block increment: %.1f" block_increment

    let sizeof_array_cap0 = test (fun mm i -> array_create mm 0)
    let sizeof_array_cap1000 = test (fun mm i -> array_create mm 1000)
    let array_overhead = sizeof_array_cap0
    let array_increment = (sizeof_array_cap1000 - sizeof_array_cap0) / 1000.0
    printfn "array overhead: %.1f" array_overhead
    printfn "array increment: %.1f" array_increment

    // We don't use String('a', 0) here because it returns shared constant object.
    let sizeof_string_len1 = test (fun mm i -> of_string mm (System.String('a', 1))) 
    let sizeof_string_len1000 = test (fun mm i -> of_string mm (System.String('a', 1000)))
    let string_increment = (sizeof_string_len1000 - sizeof_string_len1) / 999.0
    let string_overhead = sizeof_string_len1 - string_increment
    printfn "string overhead: %.1f" string_overhead
    printfn "string increment: %.1f" string_increment
    
    printfn "%.0f, %.0f, %.0f, %.0f, %.0f, %.0f, %.0f" sizeof_int sizeof_float block_overhead array_overhead block_increment string_overhead string_increment 

    printfn "Done."
    Console.ReadKey() |> ignore
    0
