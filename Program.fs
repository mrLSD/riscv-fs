module main

open ISA.RISCV
open ISA.RISCV.Utils.Bits

[<EntryPoint>]
let main argv =
    let cfg = CLI.parseCli argv CLI.InitCLI CLI.AppConfig.Default
    match cfg with
    | CLI.Failed -> printfn "Failed parse CLI params. Print --help"
    | CLI.Stopped -> ()
    | CLI.Success(x) ->
        if not x.CheckRequired then
            printfn "Wrong parameters put --help to get more information"
        else
            try
                let res = Run.Run x
                printfn "Result state: %A" res.RunState
            with ex ->
                // CheckRequired above guarantees Files is Some and non-empty here,
                // so indexing [0] is safe (the prior defensive else was dead code).
                printfn $"Error: failed to load or run '{x.Files.Value.[0]}': {ex.Message}"
    0 // return an integer exit code
