module ISA.RISCV.Decode.I

open ISA.RISCV.Utils.Bits
open ISA.RISCV.Arch

//================================================================ -- \begin_latex{Major_Opcodes}
// 'I' (Integer x32 instruction set)
type InstructionI =
    | LUI of   {| rd: Register; imm20: MachineInt |}
    | AUIPC of {| rd: Register; imm20: MachineInt |}

    | JALR of {| rd: Register; rs1: Register; imm12: MachineInt  |}
    | JAL of  {| rd: Register; imm20: MachineInt  |}

    | BEQ of  {| rs1: Register; rs2: Register; imm12: MachineInt |}
    | BNE of  {| rs1: Register; rs2: Register; imm12: MachineInt |}
    | BLT of  {| rs1: Register; rs2: Register; imm12: MachineInt |}
    | BGE of  {| rs1: Register; rs2: Register; imm12: MachineInt |}
    | BLTU of {| rs1: Register; rs2: Register; imm12: MachineInt |}
    | BGEU of {| rs1: Register; rs2: Register; imm12: MachineInt |}

    | LB of  {| rd: Register; rs1: Register; imm12: MachineInt |}
    | LH of  {| rd: Register; rs1: Register; imm12: MachineInt |}
    | LW of  {| rd: Register; rs1: Register; imm12: MachineInt |}
    | LBU of {| rd: Register; rs1: Register; imm12: MachineInt |}
    | LHU of {| rd: Register; rs1: Register; imm12: MachineInt |}

    | SB of {| rs1: Register; rs2: Register; imm12: MachineInt |}
    | SH of {| rs1: Register; rs2: Register; imm12: MachineInt |}
    | SW of {| rs1: Register; rs2: Register; imm12: MachineInt |}

    | ADDI of  {| rd: Register; rs1: Register; imm12: MachineInt  |}
    | SLTI of  {| rd: Register; rs1: Register; imm12: MachineInt  |}
    | SLTIU of {| rd: Register; rs1: Register; imm12: MachineInt  |}
    | XORI of  {| rd: Register; rs1: Register; imm12: MachineInt  |}
    | ORI of   {| rd: Register; rs1: Register; imm12: MachineInt  |}
    | ANDI of  {| rd: Register; rs1: Register; imm12: MachineInt  |}

    | SLLI of  {| rd: Register; rs1: Register; shamt: MachineInt  |}
    | SRLI of  {| rd: Register; rs1: Register; shamt: MachineInt  |}
    | SRAI of  {| rd: Register; rs1: Register; shamt: MachineInt  |}

    | ADD of  {| rd: Register; rs1: Register; rs2: Register |}
    | SUB of  {| rd: Register; rs1: Register; rs2: Register |}
    | SLL of  {| rd: Register; rs1: Register; rs2: Register |}
    | SLT of  {| rd: Register; rs1: Register; rs2: Register |}
    | SLTU of {| rd: Register; rs1: Register; rs2: Register |}
    | XOR of  {| rd: Register; rs1: Register; rs2: Register |}
    | SRL of  {| rd: Register; rs1: Register; rs2: Register |}
    | SRA of  {| rd: Register; rs1: Register; rs2: Register |}
    | OR of   {| rd: Register; rs1: Register; rs2: Register |}
    | AND of  {| rd: Register; rs1: Register; rs2: Register |}

    | FENCE of {| pred: MachineInt; succ: MachineInt; fm: MachineInt |}
    | ECALL
    | EBREAK

    | None // Instruction not found

//================================================================
// Sub-opcodes for 'I' instructions

// Sub opcode_OP_IMM.SLLI/SRLI/SRAI - 32 & 64 bit
let msbs6_SLLI      = 0b0000000L
let msbs6_SRLI      = 0b0000000L
let msbs6_SRAI      = 0b0100000L

// opcode_MISC_MEM sub-opcodes
let funct3_FENCE         = 0b000L

// opcode_SYSTEM sub-opcodes
let funct3_PRIV      = 0b000L
let funct12_ECALL    = 0b000000000000L
let funct12_EBREAK   = 0b000000000001L

/// Decode 'I' instructions
let DecodeI (instr: InstrField) : InstructionI =
    let opcode = instr.bitSlice 6   0
    // Register number can be: 0-32
    let rd     = int32(instr.bitSlice 11  7)
    let rs1    = int32(instr.bitSlice 19 15)
    let rs2    = int32(instr.bitSlice 24 20)

    let funct3 = instr.bitSlice 14 12
    let funct7 = instr.bitSlice 31 25

    // Shamt funcs
    let shamt   = instr.bitSlice 24 20
    let shamt5  = instr.bitSlice 24 20
    let shamt6  = instr.bitSlice 25 20
    // TODO: x32/64 check for Shamt
    let shamt_ok = true

    let imm12_I = (instr.bitSlice 31 20).signExtend 12
    let imm20_U = ((instr.bitSlice 31 12) <<< 12).signExtend 32

    let imm11_S =
            (
                ((instr.bitSlice 31 25) <<< 5) |||
                ( instr.bitSlice 11  7)
            ).signExtend 12

    let imm12_B =
            (
               ((instr.bitSlice 31  31) <<< 12) |||
               ((instr.bitSlice 30  25) <<< 5 ) |||
               ((instr.bitSlice 11   8) <<< 1 ) |||
               ((instr.bitSlice  7   7) <<< 11)
            ).signExtend 13

    let imm20_J =
            (
               ((instr.bitSlice  31  31) <<< 20) |||
               ((instr.bitSlice  30  21) <<<  1) |||
               ((instr.bitSlice  20  20) <<< 11) |||
               ((instr.bitSlice  19  12) <<< 12)
            ).signExtend 21

    // Fence
    let fm    = instr.bitSlice 31 28
    let pred  = instr.bitSlice 27 24
    let succ  = instr.bitSlice 23 20

    match (opcode) with
    // Upper Immediate Opcodes
    | 0b0110111L -> LUI   {| rd = rd; imm20 = imm20_U |}
    | 0b0010111L -> AUIPC {| rd = rd; imm20 = imm20_U |}

    // Jump Opcodes
    | 0b1100111L -> JALR {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
    | 0b1101111L -> JAL  {| rd = rd; imm20 = imm20_J |}

    // Branch Opcodes
    | 0b1100011L ->
        match funct3 with
        | 0b000L -> BEQ  {| rs1 = rs1; rs2 = rs2; imm12 = imm12_B |}
        | 0b001L -> BNE  {| rs1 = rs1; rs2 = rs2; imm12 = imm12_B |}
        | 0b100L -> BLT  {| rs1 = rs1; rs2 = rs2; imm12 = imm12_B |}
        | 0b101L -> BGE  {| rs1 = rs1; rs2 = rs2; imm12 = imm12_B |}
        | 0b110L -> BLTU {| rs1 = rs1; rs2 = rs2; imm12 = imm12_B |}
        | 0b111L -> BGEU {| rs1 = rs1; rs2 = rs2; imm12 = imm12_B |}
        | _      -> None

    // Load Opcodes
    | 0b0000011L ->
        match funct3 with
        | 0b000L -> LB  {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
        | 0b001L -> LH  {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
        | 0b010L -> LW  {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
        | 0b100L -> LBU {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
        | 0b101L -> LHU {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
        | _      -> None

    // Store opcodes
    | 0b0100011L ->
        match funct3 with
        | 0b000L -> SB {| rs1 = rs1; rs2 = rs2; imm12 = imm11_S |}
        | 0b001L -> SH {| rs1 = rs1; rs2 = rs2; imm12 = imm11_S |}
        | 0b010L -> SW {| rs1 = rs1; rs2 = rs2; imm12 = imm11_S |}
        | _      -> None

    // Immediate Opcodes
    | 0b0010011L ->
        match funct3 with
        | 0b000L -> ADDI  {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
        | 0b010L -> SLTI  {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
        | 0b011L -> SLTIU {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
        | 0b100L -> XORI  {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
        | 0b110L -> ORI   {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
        | 0b111L -> ANDI  {| rd = rd; rs1 = rs1; imm12 = imm12_I |}

        // Shift Immediate Opcodes
        | 0b001L when funct7 = msbs6_SLLI  -> SLLI {| rd = rd; rs1 = rs1; shamt = shamt |}
        | 0b101L when funct7 = msbs6_SRLI  -> SRLI {| rd = rd; rs1 = rs1; shamt = shamt |}
        | 0b101L when funct7 = msbs6_SRAI  -> SRAI {| rd = rd; rs1 = rs1; shamt = shamt |}
        | _      -> None

    // ALU Opcodes
    | 0b0110011L ->
        match funct3 with
        | 0b000L when funct7 = 0b0000000L -> ADD  {| rd = rd; rs1 = rs1; rs2 = rs2 |}
        | 0b000L when funct7 = 0b0100000L -> SUB  {| rd = rd; rs1 = rs1; rs2 = rs2 |}
        | 0b001L when funct7 = 0b0000000L -> SLL  {| rd = rd; rs1 = rs1; rs2 = rs2 |}
        | 0b010L when funct7 = 0b0000000L -> SLT  {| rd = rd; rs1 = rs1; rs2 = rs2 |}
        | 0b011L when funct7 = 0b0000000L -> SLTU {| rd = rd; rs1 = rs1; rs2 = rs2 |}
        | 0b100L when funct7 = 0b0000000L -> XOR  {| rd = rd; rs1 = rs1; rs2 = rs2 |}
        | 0b101L when funct7 = 0b0000000L -> SRL  {| rd = rd; rs1 = rs1; rs2 = rs2 |}
        | 0b101L when funct7 = 0b0100000L -> SRA  {| rd = rd; rs1 = rs1; rs2 = rs2 |}
        | 0b110L when funct7 = 0b0000000L -> OR   {| rd = rd; rs1 = rs1; rs2 = rs2 |}
        | 0b111L when funct7 = 0b0000000L -> AND  {| rd = rd; rs1 = rs1; rs2 = rs2 |}
        | _ -> None

    // Fence Opcode
    | 0b0001111L when rd = 0 && rs1 = 0 && funct3 = funct3_FENCE -> FENCE {| fm = fm; pred = pred; succ = succ  |}

    // System opcodes
    | 0b1110011L when rd = 0 && rs1 = 0 && funct3 = funct3_PRIV && imm12_I = funct12_ECALL  -> ECALL
    | 0b1110011L when rd = 0 && rs1 = 0 && funct3 = funct3_PRIV && imm12_I = funct12_EBREAK -> EBREAK

    | _ -> None
