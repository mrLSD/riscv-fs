module main

open ISA.RISCV
open Microsoft.FSharp.Collections
open System
open ELFSharp.ELF

let printBits (x: int64) =
    let res = System.Convert.ToString(x, 2).PadLeft(64, '0')
    printfn "Bits: %s" res

let readElfFile =
    let elf = ELFReader.Load "add32.elf"
    Array.concat [| for x in elf.GetSections() -> x.GetContents().AsMemory().ToArray() |]

[<EntryPoint>]
let main argv =
    let data = readElfFile
    printfn "Program data length: %A" data.Length
    0 // return an integer exit code
