module main

open ISA.RISCV

let printBits (x: int) =
    let res = System.Convert.ToString(x, 2).PadLeft(8, '0')
    printfn "Bits: %s" res
    
[<EntryPoint>]
let main argv =
    let x = -255
    printBits x
    x>>>5 |> printBits
    x<<<5 |> printBits
    5<<<x |> printBits
    Decode.decode_execution
    0 // return an integer exit code
