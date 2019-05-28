module main

open System

open ISA.RISCV
open ISA.RISCV.Utils.Bits

let printBits (x: int) =
    let res = System.Convert.ToString(x, 2).PadLeft(32, '0')
    printfn "Bits: %s" res

[<EntryPoint>]
let main argv =
    Decode.decode_execution
    0 // return an integer exit code
