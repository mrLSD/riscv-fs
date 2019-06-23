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

let rec fetchArr (data : byte array) (acc : uint32) =
    if data.Length > 1 then
        [| acc, data.[0] |] |> Array.append (fetchArr data.[1..] (acc - 1u))
    else
        [| (acc, data.[0]) |]

let readElfFile =
//    let elf = ELFReader.Load "add32.elf"
    let elf = ELFReader.Load "and32.elf"

    let mutable mem = Map([0u, 0uy])
    for section in elf.GetSections<ProgBitsSection<uint32>>() do
        if section.Flags.HasFlag SectionFlags.Executable then
            let content = section.GetContents()
            fetchArr content (uint32(content.Length) - 1u + uint32(section.LoadAddress))
            |> Array.iter (fun (i, v) -> printfn "0x%x | %A" i v )
//            let mutable ind = la
//            for cnt in section.GetContents() do
//                mem <- Map.add ind cnt mem
//                ind <- ind + 1u

    //Map.iter (fun k v -> printfn "%d %A" k v) mem

//    let elf = ELFReader.Load "rv32mi.elf"
//        printfn "S: %A | %A" (s.Name) (s.GetContents().Length)
    Array.concat [| for x in elf.GetSections() -> if x.Name = ".text.init" then x.GetContents() else [||] |]

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
//    let mstate = InitMachineState
//    do runCycle data mstate

    printfn "Program data length: %A" data.Length
    0 // return an integer exit code
