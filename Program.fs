module main

open ISA.RISCV

[<EntryPoint>]
let main argv =
    let cfg = CLI.parseCli argv CLI.InitCLI CLI.AppConfig.Default
    match cfg with
    | CLI.Failed -> failwith "Failed parse CLI params. Print --help"
    | CLI.Stopped -> ()
    | CLI.Success(x) ->
        if not x.CheckRequired then
            printfn "Wrong parameters put --help to get more information"
        else
            printfn "CFG: %A" cfg
    0 // return an integer exit code
