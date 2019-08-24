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

// Print log message for current instruction step
let verbosityMessage (instr : InstrField) (decodedInstr : I.InstructionI) (mstate : MachineState) =
    let typeName = decodedInstr.GetType().Name
    let instrMsg =
        match (decodedInstr) with
        | I.LUI x | I.AUIPC x -> sprintf "x%d, 0x%08x" x.rd x.imm20
        | I.JAL x -> sprintf "x%d, 0x%08x\n" x.rd x.imm20
        | I.JALR x -> sprintf "x%d, x%d, 0x%08x\n" x.rd x.rs1 x.imm12
        | I.LB x | I.LH x | I.LW x | I.LBU x | I.LHU x | I.LB x | I.ADDI x | I.SLTI x | I.XORI x | I.ORI x | I.ANDI x -> sprintf "x%d, x%d, %d" x.rd x.rs1 x.imm12
        | I.BEQ x | I.BNE x | I.BLT x | I.BGE x | I.BLTU x | I.BGEU x | I.SB x | I.SH x | I.SW x -> sprintf "x%d, x%d, 0x%08x" x.rs1 x.rs2 x.imm12
        | I.SLLI x | I.SRLI x | I.SRAI x -> sprintf "x%d, x%d, %d" x.rd x.rs1 x.shamt
        | I.ADD x | I.SUB x | I.SLL x | I.SLT x | I.SLTU x | I.XOR x | I.SRL x | I.SRA x | I.OR x | I.AND x -> sprintf "x%d, x%d, x%d" x.rd x.rs1 x.rs2
        | I.FENCE _ | I.EBREAK | I.ECALL -> ""
        | _ -> "Undef"
    let pc = sprintf "%08x:" mstate.PC
    let instr = sprintf "%08x" instr
    let instrMsg = String.Format("{0,-7}{1}", typeName, instrMsg)
    printfn "%s" (String.Format("{0,-12}{1,-12}{2}", pc, instr, instrMsg))

// Get registers state
let verbosityMessageRegisters (mstate : MachineState) =
    printfn "Not zero Registers: "
    for x in 0..31 do
        if mstate.Registers.[x] <> 0L then
            let value = sprintf "0x%x" mstate.Registers.[x]
            printfn "%s" (String.Format("\tx{0, -3}{1}", x, value))

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

            match decodedInstr with
            | I.InstructionI.None -> mstate.setRunState (Trap TrapErrors.InstructionDecode)
            | _ ->
                if mstate.Verbosity then
                    verbosityMessage instr.Value decodedInstr mstate

                ExecuteI.ExecuteI decodedInstr mstate
    match mstate.RunState with
    | Trap _ -> mstate
    | RunMachineState.Stopped ->
        verbosityMessageRegisters mstate
        mstate
    | _ -> runCycle mstate

let Run (cfg : AppConfig) =
    let data = readElfFile cfg.Files.Value.[0]
    let mstate = InitMachineState data cfg.Arch.Value cfg.Verbosity.Value
    runCycle mstate
