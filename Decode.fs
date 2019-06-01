/// Decode instructions set
 
module ISA.RISCV.Decode

open ISA.RISCV.Utils.Bits

type MachineInt = int32
type Register = MachineInt
type Opcode = MachineInt
type InstrField = MachineInt 

//================================================================ -- \begin_latex{Major_Opcodes}
// 'I' (Base instruction set)
type InstructionI =
    | Lui of   {| rd: Register; imm20: MachineInt |}
    | Auipc of {| rd: Register; imm20: MachineInt |}
        
    | Jalr of {| rd: Register; rs1: Register; imm12: MachineInt  |}
    | Jal of  {| rd: Register; imm20: MachineInt  |}

    | Beq of  {| rs1: Register; rs2: Register; imm12: MachineInt |}
    | Bne of  {| rs1: Register; rs2: Register; imm12: MachineInt |}
    | Blt of  {| rs1: Register; rs2: Register; imm12: MachineInt |}
    | Bge of  {| rs1: Register; rs2: Register; imm12: MachineInt |}
    | Bltu of {| rs1: Register; rs2: Register; imm12: MachineInt |}
    | Bgeu of {| rs1: Register; rs2: Register; imm12: MachineInt |}
        
    | Lb of  {| rd: Register; rs1: Register; imm12: MachineInt |}
    | Lh of  {| rd: Register; rs1: Register; imm12: MachineInt |}
    | Lw of  {| rd: Register; rs1: Register; imm12: MachineInt |}
    | Lbu of {| rd: Register; rs1: Register; imm12: MachineInt |}
    | Lhu of {| rd: Register; rs1: Register; imm12: MachineInt |}

    | Sb of {| rs1: Register; rs2: Register; imm12: MachineInt |}
    | Sh of {| rs1: Register; rs2: Register; imm12: MachineInt |}
    | Sw of {| rs1: Register; rs2: Register; imm12: MachineInt |}
        
    | Addi of  {| rd: Register; rs1: Register; imm12: MachineInt  |}
    | Slti of  {| rd: Register; rs1: Register; imm12: MachineInt  |}
    | Sltiu of {| rd: Register; rs1: Register; imm12: MachineInt  |}
    | Xori of  {| rd: Register; rs1: Register; imm12: MachineInt  |}
    | Ori of   {| rd: Register; rs1: Register; imm12: MachineInt  |}
    | Andi of  {| rd: Register; rs1: Register; imm12: MachineInt  |}
    
    | Slli of  {| rd: Register; rs1: Register; shamt: MachineInt  |}
    | Srli of  {| rd: Register; rs1: Register; shamt: MachineInt  |}
    | Srai of  {| rd: Register; rs1: Register; shamt: MachineInt  |}
    
    | Add of  {| rd: Register; rs1: Register; rs2: Register |}
    | Sub of  {| rd: Register; rs1: Register; rs2: Register |}
    | Sll of  {| rd: Register; rs1: Register; rs2: Register |}
    | Slt of  {| rd: Register; rs1: Register; rs2: Register |}
    | Sltu of {| rd: Register; rs1: Register; rs2: Register |}
    | Xor of  {| rd: Register; rs1: Register; rs2: Register |}
    | Srl of  {| rd: Register; rs1: Register; rs2: Register |}
    | Sra of  {| rd: Register; rs1: Register; rs2: Register |}
    | Or of   {| rd: Register; rs1: Register; rs2: Register |}
    | And of  {| rd: Register; rs1: Register; rs2: Register |}

    | Fence of {| pred: MachineInt; succ: MachineInt; fm: MachineInt |}
    | Ecall
    | Ebreak
    
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
               ((instr.bitSlice  31  31) <<< 20) |||
               ((instr.bitSlice  30  21) <<<  1) |||
               ((instr.bitSlice  20  20) <<< 11) |||
               ((instr.bitSlice  19  12) <<< 12)
    
    // Fence
    let fm    = instr.bitSlice 31 28
    let pred  = instr.bitSlice 27 24
    let succ  = instr.bitSlice 23 20
    
    match (opcode) with
    | (op) when op = opcode_LUI   -> Lui   {| rd = rd; imm20 = imm20_U |}
    | (op) when op = opcode_AUIPC -> Auipc {| rd = rd; imm20 = imm20_U |}
    
    | (op) when op = opcode_JALR -> Jalr {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
    | (op) when op = opcode_JAL  -> Jal  {| rd = rd; imm20 = imm20_J |}
    
    | (op) when op = opcode_BRANCH && funct3 = funct3_BEQ  -> Beq  {| rs1 = rs1; rs2 = rs2; imm12 = imm12_B |}
    | (op) when op = opcode_BRANCH && funct3 = funct3_BNE  -> Bne  {| rs1 = rs1; rs2 = rs2; imm12 = imm12_B |}
    | (op) when op = opcode_BRANCH && funct3 = funct3_BLT  -> Blt  {| rs1 = rs1; rs2 = rs2; imm12 = imm12_B |}
    | (op) when op = opcode_BRANCH && funct3 = funct3_BGE  -> Bge  {| rs1 = rs1; rs2 = rs2; imm12 = imm12_B |}
    | (op) when op = opcode_BRANCH && funct3 = funct3_BLTU -> Bltu {| rs1 = rs1; rs2 = rs2; imm12 = imm12_B |}
    | (op) when op = opcode_BRANCH && funct3 = funct3_BGEU -> Bgeu {| rs1 = rs1; rs2 = rs2; imm12 = imm12_B |}
    
    | (op) when op = opcode_LOAD && funct3 = funct3_LB  -> Lb  {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
    | (op) when op = opcode_LOAD && funct3 = funct3_LH  -> Lh  {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
    | (op) when op = opcode_LOAD && funct3 = funct3_LW  -> Lw  {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
    | (op) when op = opcode_LOAD && funct3 = funct3_LBU -> Lbu {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
    | (op) when op = opcode_LOAD && funct3 = funct3_LHU -> Lhu {| rd = rd; rs1 = rs1; imm12 = imm12_I |}

    | (op) when op = opcode_STORE && funct3 = funct3_SB -> Sb {| rs1 = rs1; rs2 = rs2; imm12 = imm11_S |}
    | (op) when op = opcode_STORE && funct3 = funct3_SH -> Sh {| rs1 = rs1; rs2 = rs2; imm12 = imm11_S |}
    | (op) when op = opcode_STORE && funct3 = funct3_SW -> Sw {| rs1 = rs1; rs2 = rs2; imm12 = imm11_S |}

    // OP_IMM    
    | (op) when op = opcode_OP_IMM && funct3 = funct3_ADDI  -> Addi {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
    // Shamt
    | (op) when op = opcode_OP_IMM && funct3 = funct3_SLLI && funct7 = msbs6_SLLI  -> Slli {| rd = rd; rs1 = rs1; shamt = shamt |}
    // OP
    | (op) when op = opcode_OP && funct3 = funct3_ADD && funct7 = funct7_ADD  -> Add {| rd = rd; rs1 = rs1; rs2 = rs2 |}
        
    | (op) when op = opcode_MISC_MEM && rd = 0 && rs1 = 0 && funct3 = funct3_FENCE -> Fence {| fm = fm; pred = pred; succ = succ  |}

    | (op) when op = opcode_SYSTEM && rd = 0 && rs1 = 0 && funct3 = funct3_PRIV && imm12_I = funct12_ECALL  -> Ecall
    | (op) when op = opcode_SYSTEM && rd = 0 && rs1 = 0 && funct3 = funct3_PRIV && imm12_I = funct12_EBREAK -> Ebreak
        
    | _ -> None

//type Instructions =
//    | IInsruction of InstructionI32 
    
let decode_execution =
    printfn "Decode instructions: %d" 10
