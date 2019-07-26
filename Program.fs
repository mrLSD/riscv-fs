module main

open ISA.RISCV

[<EntryPoint>]
let main argv =
    let _ = CLI.InitCLI argv
    0 // return an integer exit code
