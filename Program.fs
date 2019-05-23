module main

open ISA.RISCV
open Microsoft.FSharp.Collections

let printBits (x: int64) =
    let res = System.Convert.ToString(x, 2).PadLeft(64, '0')
    printfn "Bits: %s" res

[<EntryPoint>]
let main argv =
    let x = [| 0b0001y; 0b0011y; 0b0111y |]
    let xr = MachineState.combineBytes x
    printBits xr
    0 // return an integer exit code
