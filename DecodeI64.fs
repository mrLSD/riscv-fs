module ISA.RISCV.Decode.I64

open System
open ISA.RISCV.Utils.Bits
open ISA.RISCV.Arch
open ISA.RISCV.MachineState

//================================================================ -- \begin_latex{Major_Opcodes}
// 'I64' (Integer x64 instruction set)
type InstructionI64 =
    | LWU   of  {| rd: Register; rs1: Register; imm12: MachineInt  |}
    | LD    of  {| rd: Register; rs1: Register; imm12: MachineInt  |}
    | SD    of  {| rd: Register; rs1: Register; imm12: MachineInt  |}
    | ADDIW of  {| rd: Register; rs1: Register; imm12: MachineInt  |}

    | SLLIW of  {| rd: Register; rs1: Register; shamt: MachineInt  |}
    | SRLIW of  {| rd: Register; rs1: Register; shamt: MachineInt  |}
    | SRAIW of  {| rd: Register; rs1: Register; shamt: MachineInt  |}

    | ADDW  of  {| rd: Register; rs1: Register; rs2: Register |}
    | SUBW  of  {| rd: Register; rs1: Register; rs2: Register |}
    | SLLW  of  {| rd: Register; rs1: Register; rs2: Register |}
    | SRLW  of  {| rd: Register; rs1: Register; rs2: Register |}
    | SRAW  of  {| rd: Register; rs1: Register; rs2: Register |}

    | None // Instruction not found

/// Decode 'I64' instructions
let Decode (instr: InstrField) : InstructionI64 =
    let opcode = instr.bitSlice 6   0
    // Register number can be: 0-32
    let rd     = int32(instr.bitSlice 11  7)
    let rs1    = int32(instr.bitSlice 19 15)
    let rs2    = int32(instr.bitSlice 24 20)

    let funct3 = instr.bitSlice 14 12
    let funct7 = instr.bitSlice 31 25

    // Shamt funcs
    let shamt   = instr.bitSlice 24 20

    let imm12_I = (instr.bitSlice 31 20).signExtend 12

    let imm11_S =
            (
                ((instr.bitSlice 31 25) <<< 5) |||
                ( instr.bitSlice 11  7)
            ).signExtend 12

    match (opcode) with
    // Load Opcodes
    | 0b0000011L ->
        match funct3 with
        | 0b110L -> LWU {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
        | 0b011L -> LD  {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
        | _      -> None

    // Store opcodes
    | 0b0100011L ->
        match funct3 with
        | 0b011L -> SD  {| rd = rd; rs1 = rs1; imm12 = imm11_S |}
        | _      -> None

    | 0b0011011L ->
        match funct3 with
        // Immediate Opcodes
        | 0b000L -> ADDIW  {| rd = rd; rs1 = rs1; imm12 = imm12_I |}

        // Shift Immediate Opcodes
        | 0b001L when funct7 = 0b0000000L -> SLLIW {| rd = rd; rs1 = rs1; shamt = shamt |}
        | 0b101L when funct7 = 0b0000000L -> SRLIW {| rd = rd; rs1 = rs1; shamt = shamt |}
        | 0b101L when funct7 = 0b0100000L -> SRAIW {| rd = rd; rs1 = rs1; shamt = shamt |}

        // ALU opcodes
        | 0b000L when funct7 = 0b0000000L -> ADDW {| rd = rd; rs1 = rs1; rs2 = rs2 |}
        | 0b000L when funct7 = 0b0100000L -> SUBW {| rd = rd; rs1 = rs1; rs2 = rs2 |}
        | 0b001L when funct7 = 0b0000000L -> SLLW {| rd = rd; rs1 = rs1; rs2 = rs2 |}
        | 0b101L when funct7 = 0b0000000L -> SRLW {| rd = rd; rs1 = rs1; rs2 = rs2 |}
        | 0b101L when funct7 = 0b0100000L -> SRAW {| rd = rd; rs1 = rs1; rs2 = rs2 |}
        | _      -> None

    | _      -> None

// Current ISA print log message for current instruction step
let verbosityMessage (instr : InstrField) (decodedInstr : InstructionI64) (mstate : MachineState) =
    let typeName = decodedInstr.GetType().Name
    let instrMsg =
        match (decodedInstr) with
        | LWU x | LD x | SD x | ADDIW x-> sprintf "x%d, x%d, %d" x.rd x.rs1 x.imm12
        | SLLIW x | SRLIW x | SRAIW x -> sprintf "x%d, x%d, %d" x.rd x.rs1 x.shamt
        | ADDW x | SUBW x | SLLW x | SRLW x | SRAW x -> sprintf "x%d, x%d, x%d" x.rd x.rs1 x.rs2
        | _ -> "Undef"
    let pc = sprintf "%08x:" mstate.PC
    let instr = sprintf "%08x" instr
    let instrMsg = String.Format("{0,-7}{1}", typeName, instrMsg)
    printfn "%s" (String.Format("{0,-12}{1,-12}{2}", pc, instr, instrMsg))
