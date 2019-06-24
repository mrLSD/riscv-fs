module main

open ISA.RISCV.Run
open Microsoft.FSharp.Collections



// Help function for fetch Elf data

//    match decodedInstr with
//    | I.InstructionI.None -> ()
//    | _ -> printfn "%A\t| %A" mstate.PC decodedInstr


[<EntryPoint>]
let main argv =
    let dm = Map.ofList [(1, 2); (2, 2)]
    printfn "MAP: %A" dm
    let dm = Map.add 1 10 dm
    printfn "MAP: %A" dm

    let data = readElfFile
    data |> Map.iter (fun k v -> printfn "0x%x | %A" k v )
    0 // return an integer exit code
