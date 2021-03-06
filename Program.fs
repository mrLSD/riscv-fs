﻿module main

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
            let res = Run.Run x
            printfn "Result state: %A" res.RunState
    0 // return an integer exit code
