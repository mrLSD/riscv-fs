module main

open ISA.RISCV.Fetch
open ISA.RISCV.MachineState
open Microsoft.FSharp.Collections

open ELFSharp.ELF
open ELFSharp.ELF.Sections
open ISA.RISCV.Decode

let printBits (x: int64) =
    let res = System.Convert.ToString(x, 2).PadLeft(64, '0')
    printfn "Bits: %s" res

// Help function for fetch Elf data
let fetchIndexAddr (data : byte array) (index : uint32) =
    Array.zip [| index .. index + uint32(data.Length) - 1u |] data

let getSectionContent (section : ProgBitsSection<uint32>) =
    if section.Flags.HasFlag SectionFlags.Executable then
        fetchIndexAddr (section.GetContents()) section.LoadAddress
    else
        [||]


/// Read Elf data content to Map data with format: [address, dataByte]
let readElfFile =
//    let elf = ELFReader.Load "add32.elf"
    let elf = ELFReader.Load "and32.elf"
    Map.ofArray (Array.concat [| for s in elf.GetSections() -> getSectionContent s |])

let rec runCycle (binData : byte array) (mstate : MachineState) =
    let instr = FetchInstruction binData mstate
    let decodedInstr = I.DecodeI instr
    printfn "%A\t | 0x%x\t" mstate.PC instr

//    match decodedInstr with
//    | I.InstructionI.None -> ()
//    | _ -> printfn "%A\t| %A" mstate.PC decodedInstr

    if mstate.PC + 8 < binData.Length then
        let mstate = {mstate with PC = mstate.PC + 4}
        runCycle binData mstate

[<EntryPoint>]
let main argv =
    let data = readElfFile
    data |> Map.iter (fun k v -> printfn "0x%x | %A" k v )
//    let mstate = InitMachineState
//    do runCycle data mstate

//    printfn "Program data length: %A" Map
    0 // return an integer exit code
