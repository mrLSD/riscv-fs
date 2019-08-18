module ISA.RISCV.Run

open System

open ELFSharp.ELF
open ELFSharp.ELF.Sections

open ISA.RISCV
open ISA.RISCV.MachineState
open ISA.RISCV.Utils.Bits
open ISA.RISCV.Arch
open ISA.RISCV.CLI
open ISA.RISCV.Decode
open ISA.RISCV.Decode
open ISA.RISCV.Decode
open ISA.RISCV.Decode
open ISA.RISCV.Decode
open ISA.RISCV.Decode
open ISA.RISCV.Decode
open ISA.RISCV.Decode
open ISA.RISCV.Decode
open ISA.RISCV.Decode
open ISA.RISCV.Decode
open ISA.RISCV.Decode

let verbosityMessage (instr : InstrField) (decodedInstr : I.InstructionI) (mstate : MachineState) =
    let opcode = instr.bitSlice 6 0
    let opcodeType = match (opcode) with
                     | (op) when op = I.opcode_LUI -> "opcode_LUI"
                     | (op) when op = I.opcode_AUIPC -> "opcode_AUIPC"
                     | (op) when op = I.opcode_JALR -> "opcode_JALR"
                     | (op) when op = I.opcode_JAL -> "opcode_JAL"
                     | (op) when op = I.opcode_BRANCH -> "opcode_BRANCH"
                     | (op) when op = I.opcode_LOAD -> "opcode_LOAD"
                     | (op) when op = I.opcode_STORE -> "opcode_STORE"
                     | (op) when op = I.opcode_OP_IMM -> "opcode_OP_IMM"
                     | (op) when op = I.opcode_OP -> "opcode_OP"
                     | (op) when op = I.opcode_MISC_MEM -> "opcode_MISC_MEM"
                     | (op) when op = I.opcode_SYSTEM -> "opcode_SYSTEM"
                     | _ -> "Undef"
    printfn "%s" (String.Format("{0,-5}{1,-20}{2}", mstate.PC, instr, opcodeType))
    
// Help function for fetch Elf data
let getSectionContent (section : ProgBitsSection<uint32>) =
    let fetchIndexAddr (data : byte array) (index : int64) =
        Array.zip [| index .. index + int64(data.Length) - 1L |] data

    if section.Flags.HasFlag SectionFlags.Executable then
        fetchIndexAddr (section.GetContents()) (int64 section.LoadAddress)
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
        | None -> mstate.setRunState (Trap (InstructionFetch mstate.PC))
        | _ ->
            let decodedInstr = I.DecodeI instr.Value
            printfn "0x%x\t | %A" mstate.PC decodedInstr
            match decodedInstr with
            | I.InstructionI.None -> mstate.setRunState (Trap TrapErrors.InstructionDecode)
            | _ ->
                ExecuteI.ExecuteI decodedInstr mstate
    match mstate.RunState with
    | Trap _ -> mstate
    | _ -> runCycle mstate

let Run (cfg : AppConfig) =
    let data = readElfFile cfg.Files.Value.[0]
    let mstate = InitMachineState data cfg.Arch.Value cfg.Verbosity.Value
    runCycle mstate
