module main

let printBits (x: int) =
    let res = System.Convert.ToString(x, 2).PadLeft(32, '0')
    printfn "Bits: %s" res

[<EntryPoint>]
let main argv =
    printBits 7
    0 // return an integer exit code
