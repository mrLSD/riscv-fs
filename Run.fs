module ISA.RISCV.Run

open ELFSharp.ELF
open ELFSharp.ELF.Sections

open ISA.RISCV.MachineState
open ISA.RISCV.Utils.Bits
open ISA.RISCV.Arch
open ISA.RISCV.CLI
open ISA.RISCV.Decode

// Help function for fetch Elf data
let getSectionContent (section : ProgBitsSection<uint32>) =
    let fetchIndexAddr (data : byte array) (index : uint32) =
        Array.zip [| index .. index + uint32(data.Length) - 1u |] data

    if section.Flags.HasFlag SectionFlags.Executable then
        fetchIndexAddr (section.GetContents()) section.LoadAddress
    else
        [||]


/// Read Elf data content to Map data with format: [address, dataByte]
let readElfFile file =
//    let elf = ELFReader.Load "add32.elf"
//    let elf = ELFReader.Load "and32.elf"
    let elf = ELFReader.Load file
    Map.ofArray (Array.concat [| for s in elf.GetSections() -> getSectionContent s |])

let fetchInstruction (mstate : MachineState) : InstrField option =
    loadWord mstate.Memory mstate.PC

let rec runCycle (mstate : MachineState) =
    let instr = fetchInstruction mstate
    let mstate =
        match instr with
        | None -> mstate.setRunState (Trap TrapErrors.InstructionFetch)
        | _ ->
            let decodedInstr = I.DecodeI instr.Value
            printfn "0x%x\t | %A" mstate.PC decodedInstr
            match decodedInstr with
            | I.InstructionI.None -> mstate.setRunState (Trap TrapErrors.InstructionDecode)
            | _ -> mstate
    match mstate.RunState with
    | Trap _ -> mstate
    | _ ->
        let mstate = mstate.incPC 4u
        runCycle mstate

let Run (cfg : AppConfig) =
    let data = readElfFile cfg.Files.Value.[0]
    let mstate = InitMachineState data cfg.Verbosity.Value
    runCycle mstate
