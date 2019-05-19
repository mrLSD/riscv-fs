module main

open Microsoft.FSharp.Collections

let printBits (x: int) =
    let res = System.Convert.ToString(x, 2).PadLeft(32, '0')
    printfn "Bits: %s" res

[<EntryPoint>]
let main argv =
    0 // return an integer exit code
