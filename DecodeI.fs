module ISA.RISCV.Decode.I

open System
open ISA.RISCV.Utils.Bits
open ISA.RISCV.Arch
open ISA.RISCV.MachineState

//================================================================
// 'I' (Integer x32 instruction set)
type InstructionI =
    | LUI of   {| rd: Register; imm20: InstrField |}
    | AUIPC of {| rd: Register; imm20: InstrField |}

    | JALR of {| rd: Register; rs1: Register; imm12: InstrField  |}
    | JAL of  {| rd: Register; imm20: InstrField  |}

    | BEQ of  {| rs1: Register; rs2: Register; imm12: InstrField |}
    | BNE of  {| rs1: Register; rs2: Register; imm12: InstrField |}
    | BLT of  {| rs1: Register; rs2: Register; imm12: InstrField |}
    | BGE of  {| rs1: Register; rs2: Register; imm12: InstrField |}
    | BLTU of {| rs1: Register; rs2: Register; imm12: InstrField |}
    | BGEU of {| rs1: Register; rs2: Register; imm12: InstrField |}

    | LB of  {| rd: Register; rs1: Register; imm12: InstrField |}
    | LH of  {| rd: Register; rs1: Register; imm12: InstrField |}
    | LW of  {| rd: Register; rs1: Register; imm12: InstrField |}
    | LBU of {| rd: Register; rs1: Register; imm12: InstrField |}
    | LHU of {| rd: Register; rs1: Register; imm12: InstrField |}

    | SB of {| rs1: Register; rs2: Register; imm12: InstrField |}
    | SH of {| rs1: Register; rs2: Register; imm12: InstrField |}
    | SW of {| rs1: Register; rs2: Register; imm12: InstrField |}

    | ADDI of  {| rd: Register; rs1: Register; imm12: InstrField  |}
    | SLTI of  {| rd: Register; rs1: Register; imm12: InstrField  |}
    | SLTIU of {| rd: Register; rs1: Register; imm12: InstrField  |}
    | XORI of  {| rd: Register; rs1: Register; imm12: InstrField  |}
    | ORI of   {| rd: Register; rs1: Register; imm12: InstrField  |}
    | ANDI of  {| rd: Register; rs1: Register; imm12: InstrField  |}

    | SLLI of  {| rd: Register; rs1: Register; shamt: InstrField  |}
    | SRLI of  {| rd: Register; rs1: Register; shamt: InstrField  |}
    | SRAI of  {| rd: Register; rs1: Register; shamt: InstrField  |}

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

    | FENCE of {| pred: InstrField; succ: InstrField; fm: InstrField |}
    | ECALL
    | EBREAK

    | None // Instruction not found

/// Decode 'I' instructions
let Decode (mstate : MachineState) (instr: InstrField) : InstructionI =
    let opcode = instr.bitSlice 6   0
    // Register number can be: 0-32
    let rd     = int32(instr.bitSlice 11  7)
    let rs1    = int32(instr.bitSlice 19 15)
    let rs2    = int32(instr.bitSlice 24 20)

    let funct3 = instr.bitSlice 14 12
    let funct7 = instr.bitSlice 31 25

    // Shamt funcs
    let shamt =
        if mstate.Arch.archBits = RV32 then
            instr.bitSlice 24 20
        else
            instr.bitSlice 24 20
    let funct6 = instr.bitSlice 31 26
    let shamt_ok =
        ((instr.bitSlice 25 25) = 0) ||
        (mstate.Arch.archBits = RV64)

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
    | 0b0110111 -> LUI   {| rd = rd; imm20 = imm20_U |}
    | 0b0010111 -> AUIPC {| rd = rd; imm20 = imm20_U |}

    // Jump Opcodes
    | 0b1100111 -> JALR {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
    | 0b1101111 -> JAL  {| rd = rd; imm20 = imm20_J |}

    // Branch Opcodes
    | 0b1100011 ->
        match funct3 with
        | 0b000 -> BEQ  {| rs1 = rs1; rs2 = rs2; imm12 = imm12_B |}
        | 0b001 -> BNE  {| rs1 = rs1; rs2 = rs2; imm12 = imm12_B |}
        | 0b100 -> BLT  {| rs1 = rs1; rs2 = rs2; imm12 = imm12_B |}
        | 0b101 -> BGE  {| rs1 = rs1; rs2 = rs2; imm12 = imm12_B |}
        | 0b110 -> BLTU {| rs1 = rs1; rs2 = rs2; imm12 = imm12_B |}
        | 0b111 -> BGEU {| rs1 = rs1; rs2 = rs2; imm12 = imm12_B |}
        | _      -> None

    // Load Opcodes
    | 0b0000011 ->
        match funct3 with
        | 0b000 -> LB  {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
        | 0b001 -> LH  {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
        | 0b010 -> LW  {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
        | 0b100 -> LBU {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
        | 0b101 -> LHU {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
        | _      -> None

    // Store opcodes
    | 0b0100011 ->
        match funct3 with
        | 0b000 -> SB {| rs1 = rs1; rs2 = rs2; imm12 = imm11_S |}
        | 0b001 -> SH {| rs1 = rs1; rs2 = rs2; imm12 = imm11_S |}
        | 0b010 -> SW {| rs1 = rs1; rs2 = rs2; imm12 = imm11_S |}
        | _      -> None

    // Immediate Opcodes
    | 0b0010011 ->
        match funct3 with
        | 0b000 -> ADDI  {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
        | 0b010 -> SLTI  {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
        | 0b011 -> SLTIU {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
        | 0b100 -> XORI  {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
        | 0b110 -> ORI   {| rd = rd; rs1 = rs1; imm12 = imm12_I |}
        | 0b111 -> ANDI  {| rd = rd; rs1 = rs1; imm12 = imm12_I |}

        // Shift Immediate Opcodes
        | 0b001 when funct6 = 0b000000 && shamt_ok -> SLLI {| rd = rd; rs1 = rs1; shamt = shamt |}
        | 0b101 when funct6 = 0b000000 && shamt_ok -> SRLI {| rd = rd; rs1 = rs1; shamt = shamt |}
        | 0b101 when funct6 = 0b010000 && shamt_ok -> SRAI {| rd = rd; rs1 = rs1; shamt = shamt |}
        | _      -> None

    // ALU Opcodes
    | 0b0110011 ->
        match funct3 with
        | 0b000 when funct7 = 0b0000000 -> ADD  {| rd = rd; rs1 = rs1; rs2 = rs2 |}
        | 0b000 when funct7 = 0b0100000 -> SUB  {| rd = rd; rs1 = rs1; rs2 = rs2 |}
        | 0b001 when funct7 = 0b0000000 -> SLL  {| rd = rd; rs1 = rs1; rs2 = rs2 |}
        | 0b010 when funct7 = 0b0000000 -> SLT  {| rd = rd; rs1 = rs1; rs2 = rs2 |}
        | 0b011 when funct7 = 0b0000000 -> SLTU {| rd = rd; rs1 = rs1; rs2 = rs2 |}
        | 0b100 when funct7 = 0b0000000 -> XOR  {| rd = rd; rs1 = rs1; rs2 = rs2 |}
        | 0b101 when funct7 = 0b0000000 -> SRL  {| rd = rd; rs1 = rs1; rs2 = rs2 |}
        | 0b101 when funct7 = 0b0100000 -> SRA  {| rd = rd; rs1 = rs1; rs2 = rs2 |}
        | 0b110 when funct7 = 0b0000000 -> OR   {| rd = rd; rs1 = rs1; rs2 = rs2 |}
        | 0b111 when funct7 = 0b0000000 -> AND  {| rd = rd; rs1 = rs1; rs2 = rs2 |}
        | _ -> None

    // Fence Opcode
    | 0b0001111 when rd = 0 && rs1 = 0 && funct3 = 0b000 -> FENCE {| fm = fm; pred = pred; succ = succ  |}

    // System opcodes
    | 0b1110011 when rd = 0 && rs1 = 0 && funct3 = 0b000 && imm12_I = 0b000000000000  -> ECALL
    | 0b1110011 when rd = 0 && rs1 = 0 && funct3 = 0b000 && imm12_I = 0b000000000001 -> EBREAK

    | _ -> None

// Current ISA print log message for current instruction step
let verbosityMessage (instr : InstrField) (decodedInstr : InstructionI) (mstate : MachineState) =
    let typeName = decodedInstr.GetType().Name
    let instrMsg =
        match (decodedInstr) with
        | LUI x | AUIPC x -> sprintf "x%d, 0x%08x" x.rd x.imm20
        | JAL x -> sprintf "x%d, 0x%08x\n" x.rd x.imm20
        | JALR x -> sprintf "x%d, x%d, 0x%08x\n" x.rd x.rs1 x.imm12
        | LB x | LH x | LW x | LBU x | LHU x | LB x | ADDI x | SLTI x | XORI x | ORI x | ANDI x -> sprintf "x%d, x%d, %d" x.rd x.rs1 x.imm12
        | BEQ x | BNE x | BLT x | BGE x | BLTU x | BGEU x | SB x | SH x | SW x -> sprintf "x%d, x%d, 0x%08x" x.rs1 x.rs2 x.imm12
        | SLLI x | SRLI x | SRAI x -> sprintf "x%d, x%d, %d" x.rd x.rs1 x.shamt
        | ADD x | SUB x | SLL x | SLT x | SLTU x | XOR x | SRL x | SRA x | OR x | AND x -> sprintf "x%d, x%d, x%d" x.rd x.rs1 x.rs2
        | FENCE _ | EBREAK | ECALL -> ""
        | _ -> "Undef"
    let pc = sprintf "%08x:" mstate.PC
    let instr = sprintf "%08x" instr
    let instrMsg = String.Format("{0,-7}{1}", typeName, instrMsg)
    printfn "%s" (String.Format("{0,-12}{1,-12}{2}", pc, instr, instrMsg))
