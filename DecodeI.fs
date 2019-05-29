module ISA.RISCV.DecodeI

open ISA.RISCV.Decode
open ISA.RISCV.Utils.Bits

//================================================================ -- \begin_latex{Major_Opcodes}
// 'I' (Base instruction set)
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

//================================================================ -- \begin_latex{Major_Opcodes}
// Major Opcodes
let opcode_LUI       = 0b0110111
let opcode_AUIPC     = 0b0010111
let opcode_JAL       = 0b1101111
let opcode_JALR      = 0b1100111
let opcode_BRANCH    = 0b1100011
let opcode_LOAD      = 0b0000011
let opcode_STORE     = 0b0100011
let opcode_OP_IMM    = 0b0010011
let opcode_OP        = 0b0110011
let opcode_MISC_MEM  = 0b0001111
let opcode_SYSTEM    = 0b1110011

//================================================================
// Sub-opcodes for 'I' instructions

// opcode_JALR sub-opcodes
let funct3_JALR      = 0b000

// opcode_BRANCH sub-opcodes
let funct3_BEQ       = 0b000
let funct3_BNE       = 0b001
let funct3_BLT       = 0b100
let funct3_BGE       = 0b101
let funct3_BLTU      = 0b110
let funct3_BGEU      = 0b111

// opcode_LOAD sub-opcodes
let funct3_LB        = 0b000
let funct3_LH        = 0b001
let funct3_LW        = 0b010
let funct3_LD        = 0b011
let funct3_LBU       = 0b100
let funct3_LHU       = 0b101

// opcode_STORE sub-opcodes
let funct3_SB        = 0b000
let funct3_SH        = 0b001
let funct3_SW        = 0b010

 // opcode_OP_IMM sub-opcodes
let funct3_ADDI      = 0b000
let funct3_SLTI      = 0b010
let funct3_SLTIU     = 0b011
let funct3_XORI      = 0b100
let funct3_ORI       = 0b110
let funct3_ANDI      = 0b111

let funct3_SLLI      = 0b001
let funct3_SRLI      = 0b101
let funct3_SRAI      = 0b101

// opcode_OP_IMM.SLLI/SRLI/SRAI - 32 & 64 bit
let msbs6_SLLI      = 0b000000
let msbs6_SRLI      = 0b000000
let msbs6_SRAI      = 0b010000

// opcode_OP sub-opcodes
let funct3_ADD       = 0b000
let funct7_ADD       = 0b0000000

let funct3_SUB       = 0b000
let funct7_SUB       = 0b0100000

let funct3_SLL       = 0b001
let funct7_SLL       = 0b0000000

let funct3_SLT       = 0b010
let funct7_SLT       = 0b0000000

let funct3_SLTU      = 0b011
let funct7_SLTU      = 0b0000000

let funct3_XOR       = 0b100
let funct7_XOR       = 0b0000000

let funct3_SRL       = 0b101
let funct7_SRL       = 0b0000000

let funct3_SRA       = 0b101
let funct7_SRA       = 0b0100000

let funct3_OR        = 0b110
let funct7_OR        = 0b0000000

let funct3_AND       = 0b111
let funct7_AND       = 0b0000000

// opcode_MISC_MEM sub-opcodes
let funct3_FENCE         = 0b000

// opcode_SYSTEM sub-opcodes
let funct3_PRIV      = 0b000
let funct12_ECALL    = 0b000000000000
let funct12_EBREAK   = 0b000000000001

/// Decode 'I' instructions
let DecodeI (instr: InstrField) : InstructionI =
    let opcode = instr.bitSlice 6   0
    let rd     = instr.bitSlice 11  7
    let funct3 = instr.bitSlice 14 12
    let rs1    = instr.bitSlice 19 15
    let rs2    = instr.bitSlice 24 20
    let funct7 = instr.bitSlice 31 25

    // Shamt funcs
    let shamt   = instr.bitSlice 24 20
    let shamt5  = instr.bitSlice 24 20
    let shamt6  = instr.bitSlice 25 20
    // TODO: x32/64 check for Shamt
    let shamt_ok = true

    let imm12_I = instr.bitSlice 31 20
    let imm20_U = instr.bitSlice 31 12

    let imm11_S =
                ((instr.bitSlice 31 25) <<< 5) |||
                ( instr.bitSlice 11  7)

    let imm12_B =
               ((instr.bitSlice 31  31) <<< 12) |||
               ((instr.bitSlice 30  25) <<< 5 ) |||
               ((instr.bitSlice 11   8) <<< 1 ) |||
               ((instr.bitSlice  7   7) <<< 11)

    let imm20_J =
               (((instr.bitSlice  31  31) <<< 20) |||
               ((instr.bitSlice  30  21) <<<  1) |||
               ((instr.bitSlice  20  20) <<< 11) |||
               ((instr.bitSlice  19  12) <<< 12)
                ).signExtend 21

    // Fence
    let fm    = instr.bitSlice 31 28
    let pred  = instr.bitSlice 27 24
    let succ  = instr.bitSlice 23 20

    match (opcode) with
    | (op) when op = opcode_LUI   -> LUI   {| rd = rd; imm20 = imm20_U |}
    | (op) when op = opcode_AUIPC -> AUIPC {| rd = rd; imm20 = imm20_U |}

    | (op) when op = opcode_JALR -> JALR {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
    | (op) when op = opcode_JAL  -> JAL  {| rd = rd; imm20 = imm20_J |}

    | (op) when op = opcode_BRANCH && funct3 = funct3_BEQ  -> BEQ  {| rs1 = rs1; rs2 = rs2; imm12 = imm12_B |}
    | (op) when op = opcode_BRANCH && funct3 = funct3_BNE  -> BNE  {| rs1 = rs1; rs2 = rs2; imm12 = imm12_B |}
    | (op) when op = opcode_BRANCH && funct3 = funct3_BLT  -> BLT  {| rs1 = rs1; rs2 = rs2; imm12 = imm12_B |}
    | (op) when op = opcode_BRANCH && funct3 = funct3_BGE  -> BGE  {| rs1 = rs1; rs2 = rs2; imm12 = imm12_B |}
    | (op) when op = opcode_BRANCH && funct3 = funct3_BLTU -> BLTU {| rs1 = rs1; rs2 = rs2; imm12 = imm12_B |}
    | (op) when op = opcode_BRANCH && funct3 = funct3_BGEU -> BGEU {| rs1 = rs1; rs2 = rs2; imm12 = imm12_B |}

    | (op) when op = opcode_LOAD && funct3 = funct3_LB  -> LB  {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
    | (op) when op = opcode_LOAD && funct3 = funct3_LH  -> LH  {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
    | (op) when op = opcode_LOAD && funct3 = funct3_LW  -> LW  {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
    | (op) when op = opcode_LOAD && funct3 = funct3_LBU -> LBU {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
    | (op) when op = opcode_LOAD && funct3 = funct3_LHU -> LHU {| rd = rd; rs1 = rs1; imm12 = imm12_I |}

    | (op) when op = opcode_STORE && funct3 = funct3_SB -> SB {| rs1 = rs1; rs2 = rs2; imm12 = imm11_S |}
    | (op) when op = opcode_STORE && funct3 = funct3_SH -> SH {| rs1 = rs1; rs2 = rs2; imm12 = imm11_S |}
    | (op) when op = opcode_STORE && funct3 = funct3_SW -> SW {| rs1 = rs1; rs2 = rs2; imm12 = imm11_S |}

    | (op) when op = opcode_OP_IMM && funct3 = funct3_ADDI  -> ADDI  {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
    | (op) when op = opcode_OP_IMM && funct3 = funct3_SLTI  -> SLTI  {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
    | (op) when op = opcode_OP_IMM && funct3 = funct3_SLTIU -> SLTIU {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
    | (op) when op = opcode_OP_IMM && funct3 = funct3_XORI  -> XORI  {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
    | (op) when op = opcode_OP_IMM && funct3 = funct3_ORI   -> ORI   {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
    | (op) when op = opcode_OP_IMM && funct3 = funct3_ANDI  -> ANDI  {| rd = rd; rs1 = rs1; imm12 = imm12_I |}

    | (op) when op = opcode_OP_IMM && funct3 = funct3_SLLI && funct7 = msbs6_SLLI  -> SLLI {| rd = rd; rs1 = rs1; shamt = shamt |}
    | (op) when op = opcode_OP_IMM && funct3 = funct3_SLLI && funct7 = msbs6_SRLI  -> SRLI {| rd = rd; rs1 = rs1; shamt = shamt |}
    | (op) when op = opcode_OP_IMM && funct3 = funct3_SLLI && funct7 = msbs6_SRAI  -> SRAI {| rd = rd; rs1 = rs1; shamt = shamt |}

    | (op) when op = opcode_OP && funct3 = funct3_ADD && funct7 = funct7_ADD   -> ADD  {| rd = rd; rs1 = rs1; rs2 = rs2 |}
    | (op) when op = opcode_OP && funct3 = funct3_SUB && funct7 = funct7_SUB   -> SUB  {| rd = rd; rs1 = rs1; rs2 = rs2 |}
    | (op) when op = opcode_OP && funct3 = funct3_SLL && funct7 = funct7_SLL   -> SLL  {| rd = rd; rs1 = rs1; rs2 = rs2 |}
    | (op) when op = opcode_OP && funct3 = funct3_SLT && funct7 = funct7_SLT   -> SLT  {| rd = rd; rs1 = rs1; rs2 = rs2 |}
    | (op) when op = opcode_OP && funct3 = funct3_SLTU && funct7 = funct7_SLTU -> SLTU {| rd = rd; rs1 = rs1; rs2 = rs2 |}
    | (op) when op = opcode_OP && funct3 = funct3_XOR && funct7 = funct7_XOR   -> XOR  {| rd = rd; rs1 = rs1; rs2 = rs2 |}
    | (op) when op = opcode_OP && funct3 = funct3_SRL && funct7 = funct7_SRL   -> SRL  {| rd = rd; rs1 = rs1; rs2 = rs2 |}
    | (op) when op = opcode_OP && funct3 = funct3_SRA && funct7 = funct7_SRA   -> SRA  {| rd = rd; rs1 = rs1; rs2 = rs2 |}
    | (op) when op = opcode_OP && funct3 = funct3_SRA && funct7 = funct7_OR    -> OR   {| rd = rd; rs1 = rs1; rs2 = rs2 |}
    | (op) when op = opcode_OP && funct3 = funct3_SRA && funct7 = funct7_AND   -> AND  {| rd = rd; rs1 = rs1; rs2 = rs2 |}

    | (op) when op = opcode_MISC_MEM && rd = 0 && rs1 = 0 && funct3 = funct3_FENCE -> FENCE {| fm = fm; pred = pred; succ = succ  |}

    | (op) when op = opcode_SYSTEM && rd = 0 && rs1 = 0 && funct3 = funct3_PRIV && imm12_I = funct12_ECALL  -> ECALL
    | (op) when op = opcode_SYSTEM && rd = 0 && rs1 = 0 && funct3 = funct3_PRIV && imm12_I = funct12_EBREAK -> EBREAK

    | _ -> None
