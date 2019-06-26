module main
open ISA.RISCV

[<EntryPoint>]
let main argv =
    let _ = CLI.InitCLI
    0 // return an integer exit code
