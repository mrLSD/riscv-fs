module main

open ISA.RISCV

[<EntryPoint>]
let main argv =
    let str = "test"
    printfn "%s" str.[..0]

    let _ = CLI.InitCLI argv
    0 // return an integer exit code
