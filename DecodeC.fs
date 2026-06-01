module ISA.RISCV.Decode.C

open System
open ISA.RISCV.Utils.Bits
open ISA.RISCV.Arch
open ISA.RISCV.MachineState

//================================================================
// 'C'  (Compressed 16-bit instructions 'C' Standard Extension)
// Each compressed instruction is the short form of a base instruction;
// Decode expands the operands so Execute can reuse the base semantics.
// FP-compressed (C.FLD/C.FSD/C.FLW/C.FSW, *SP) belong to Zcf/Zcd and
// require F/D — on an arch without F/D they are reserved and decode to None.
type InstructionC =
    | C_ADDI4SPN of {| rd: Register; imm: InstrField |}
    | C_LW       of {| rd: Register; rs1: Register; imm: InstrField |}
    | C_LD       of {| rd: Register; rs1: Register; imm: InstrField |}
    | C_SW       of {| rs1: Register; rs2: Register; imm: InstrField |}
    | C_SD       of {| rs1: Register; rs2: Register; imm: InstrField |}
    | C_ADDI     of {| rd: Register; imm: InstrField |}
    | C_ADDIW    of {| rd: Register; imm: InstrField |}
    | C_LI       of {| rd: Register; imm: InstrField |}
    | C_LUI      of {| rd: Register; imm: InstrField |}
    | C_ADDI16SP of {| imm: InstrField |}
    | C_SRLI     of {| rd: Register; shamt: InstrField |}
    | C_SRAI     of {| rd: Register; shamt: InstrField |}
    | C_ANDI     of {| rd: Register; imm: InstrField |}
    | C_SUB      of {| rd: Register; rs2: Register |}
    | C_XOR      of {| rd: Register; rs2: Register |}
    | C_OR       of {| rd: Register; rs2: Register |}
    | C_AND      of {| rd: Register; rs2: Register |}
    | C_SUBW     of {| rd: Register; rs2: Register |}
    | C_ADDW     of {| rd: Register; rs2: Register |}
    | C_J        of {| imm: InstrField |}
    | C_JAL      of {| imm: InstrField |}
    | C_BEQZ     of {| rs1: Register; imm: InstrField |}
    | C_BNEZ     of {| rs1: Register; imm: InstrField |}
    | C_SLLI     of {| rd: Register; shamt: InstrField |}
    | C_LWSP     of {| rd: Register; imm: InstrField |}
    | C_LDSP     of {| rd: Register; imm: InstrField |}
    | C_JR       of {| rs1: Register |}
    | C_JALR     of {| rs1: Register |}
    | C_MV       of {| rd: Register; rs2: Register |}
    | C_ADD      of {| rd: Register; rs2: Register |}
    | C_EBREAK
    | C_SWSP     of {| rs2: Register; imm: InstrField |}
    | C_SDSP     of {| rs2: Register; imm: InstrField |}
    | None // Instruction not found

// CJ-format immediate (C.J / C.JAL), sign-extended 12-bit
let private cjImm (instr : InstrField) : InstrField =
    ((instr.bitSlice 12 12 <<< 11) ||| (instr.bitSlice  8  8 <<< 10) ||| (instr.bitSlice 10  9 <<< 8)
     ||| (instr.bitSlice  6  6 <<< 7) ||| (instr.bitSlice  7  7 <<< 6) ||| (instr.bitSlice  2  2 <<< 5)
     ||| (instr.bitSlice 11 11 <<< 4) ||| (instr.bitSlice  5  3 <<< 1)).signExtend 12

// CB-format immediate (C.BEQZ / C.BNEZ), sign-extended 9-bit
let private cbImm (instr : InstrField) : InstrField =
    ((instr.bitSlice 12 12 <<< 8) ||| (instr.bitSlice 6 5 <<< 6) ||| (instr.bitSlice 2 2 <<< 5)
     ||| (instr.bitSlice 11 10 <<< 3) ||| (instr.bitSlice 4 3 <<< 1)).signExtend 9

/// Decode 'C' (compressed) instructions
let Decode (mstate : MachineState) (instr : InstrField) : InstructionC =
    let rv64   = mstate.Arch.archBits = RV64
    let op     = instr.bitSlice 1 0
    let funct3 = instr.bitSlice 15 13
    // 3-bit compressed register operands map to x8..x15
    let rdc    = (instr.bitSlice 4 2) + 8     // rd'/rs2' field at [4:2]
    let rs1c   = (instr.bitSlice 9 7) + 8     // rs1'/rd'  field at [9:7]
    // wide register operands
    let rdw    = instr.bitSlice 11 7
    let rs2w   = instr.bitSlice 6 2
    // shamt for CI/CB shifts (6-bit; RV32 requires bit 5 = 0)
    let shamt  = (instr.bitSlice 12 12 <<< 5) ||| instr.bitSlice 6 2
    let shamtOk = rv64 || (instr.bitSlice 12 12) = 0
    let imm6   = ((instr.bitSlice 12 12 <<< 5) ||| instr.bitSlice 6 2).signExtend 6

    match op with
    // ---- Quadrant 0 ----
    | 0b00 ->
        match funct3 with
        | 0b000 ->   // C.ADDI4SPN -> addi rd', x2, nzuimm
            let imm = (instr.bitSlice 10 7 <<< 6) ||| (instr.bitSlice 12 11 <<< 4)
                      ||| (instr.bitSlice 5 5 <<< 3) ||| (instr.bitSlice 6 6 <<< 2)
            if imm = 0 then None else C_ADDI4SPN {| rd = rdc; imm = imm |}
        | 0b010 ->   // C.LW
            let imm = (instr.bitSlice 5 5 <<< 6) ||| (instr.bitSlice 12 10 <<< 3) ||| (instr.bitSlice 6 6 <<< 2)
            C_LW {| rd = rdc; rs1 = rs1c; imm = imm |}
        | 0b110 ->   // C.SW
            let imm = (instr.bitSlice 5 5 <<< 6) ||| (instr.bitSlice 12 10 <<< 3) ||| (instr.bitSlice 6 6 <<< 2)
            C_SW {| rs1 = rs1c; rs2 = rdc; imm = imm |}
        | 0b011 when rv64 ->   // C.LD
            let imm = (instr.bitSlice 6 5 <<< 6) ||| (instr.bitSlice 12 10 <<< 3)
            C_LD {| rd = rdc; rs1 = rs1c; imm = imm |}
        | 0b111 when rv64 ->   // C.SD
            let imm = (instr.bitSlice 6 5 <<< 6) ||| (instr.bitSlice 12 10 <<< 3)
            C_SD {| rs1 = rs1c; rs2 = rdc; imm = imm |}
        | _ -> None

    // ---- Quadrant 1 ----
    | 0b01 ->
        match funct3 with
        | 0b000 -> C_ADDI {| rd = rdw; imm = imm6 |}   // C.NOP / C.ADDI
        | 0b001 when not rv64 -> C_JAL {| imm = cjImm instr |}   // C.JAL (RV32)
        | 0b001 -> if rdw = 0 then None else C_ADDIW {| rd = rdw; imm = imm6 |}   // C.ADDIW (RV64)
        | 0b010 -> C_LI {| rd = rdw; imm = imm6 |}     // C.LI
        | 0b011 ->   // C.ADDI16SP (rd=2) / C.LUI (rd!=2)
            if rdw = 2 then
                let imm = ((instr.bitSlice 12 12 <<< 9) ||| (instr.bitSlice 4 3 <<< 7)
                           ||| (instr.bitSlice 5 5 <<< 6) ||| (instr.bitSlice 2 2 <<< 5)
                           ||| (instr.bitSlice 6 6 <<< 4)).signExtend 10
                if imm = 0 then None else C_ADDI16SP {| imm = imm |}
            // nzimm=0 is reserved; rd=x0 (nzimm!=0) is a HINT -> decode as C_LUI rd=0,
            // which executes as a write to x0 (no-op), per the C-extension HINT rules.
            elif imm6 = 0 then None
            else C_LUI {| rd = rdw; imm = imm6 <<< 12 |}
        | 0b100 ->   // misc-ALU
            match instr.bitSlice 11 10 with
            | 0b00 -> if shamtOk then C_SRLI {| rd = rs1c; shamt = shamt |} else None
            | 0b01 -> if shamtOk then C_SRAI {| rd = rs1c; shamt = shamt |} else None
            | 0b10 -> C_ANDI {| rd = rs1c; imm = imm6 |}
            | _ ->   // 0b11: CA-format register-register
                match instr.bitSlice 12 12, instr.bitSlice 6 5 with
                | 0, 0b00 -> C_SUB {| rd = rs1c; rs2 = rdc |}
                | 0, 0b01 -> C_XOR {| rd = rs1c; rs2 = rdc |}
                | 0, 0b10 -> C_OR  {| rd = rs1c; rs2 = rdc |}
                | 0, 0b11 -> C_AND {| rd = rs1c; rs2 = rdc |}
                | 1, 0b00 when rv64 -> C_SUBW {| rd = rs1c; rs2 = rdc |}
                | 1, 0b01 when rv64 -> C_ADDW {| rd = rs1c; rs2 = rdc |}
                | _ -> None
        | 0b101 -> C_J {| imm = cjImm instr |}
        | 0b110 -> C_BEQZ {| rs1 = rs1c; imm = cbImm instr |}
        | _     -> C_BNEZ {| rs1 = rs1c; imm = cbImm instr |}   // funct3 = 0b111

    // ---- Quadrant 2 ----
    | 0b10 ->
        match funct3 with
        | 0b000 -> if shamtOk then C_SLLI {| rd = rdw; shamt = shamt |} else None   // C.SLLI
        | 0b010 ->   // C.LWSP (rd != 0)
            let imm = (instr.bitSlice 3 2 <<< 6) ||| (instr.bitSlice 12 12 <<< 5) ||| (instr.bitSlice 6 4 <<< 2)
            if rdw = 0 then None else C_LWSP {| rd = rdw; imm = imm |}
        | 0b011 when rv64 ->   // C.LDSP (rd != 0)
            let imm = (instr.bitSlice 4 2 <<< 6) ||| (instr.bitSlice 12 12 <<< 5) ||| (instr.bitSlice 6 5 <<< 3)
            if rdw = 0 then None else C_LDSP {| rd = rdw; imm = imm |}
        | 0b100 ->   // CR-format: C.JR / C.MV / C.EBREAK / C.JALR / C.ADD
            match instr.bitSlice 12 12, rdw, rs2w with
            | 0, _, 0 -> if rdw = 0 then None else C_JR {| rs1 = rdw |}
            | 0, _, _ -> C_MV {| rd = rdw; rs2 = rs2w |}
            | _, 0, 0 -> C_EBREAK
            | _, _, 0 -> C_JALR {| rs1 = rdw |}
            | _, _, _ -> C_ADD {| rd = rdw; rs2 = rs2w |}
        | 0b110 ->   // C.SWSP
            let imm = (instr.bitSlice 8 7 <<< 6) ||| (instr.bitSlice 12 9 <<< 2)
            C_SWSP {| rs2 = rs2w; imm = imm |}
        | 0b111 when rv64 ->   // C.SDSP
            let imm = (instr.bitSlice 9 7 <<< 6) ||| (instr.bitSlice 12 10 <<< 3)
            C_SDSP {| rs2 = rs2w; imm = imm |}
        | _ -> None

    | _ -> None

// Current ISA print log message for current instruction step
let verbosityMessage (instr : InstrField) (decodedInstr : InstructionC) (mstate : MachineState) =
    let typeName = decodedInstr.GetType().Name
    let instrMsg =
        match decodedInstr with
        | C_ADDI4SPN x | C_ADDI x | C_ADDIW x | C_LI x | C_LUI x
        | C_ANDI x | C_LWSP x | C_LDSP x -> sprintf "x%d, %d" x.rd x.imm
        | C_LW x | C_LD x -> sprintf "x%d, %d(x%d)" x.rd x.imm x.rs1
        | C_SW x | C_SD x -> sprintf "x%d, %d(x%d)" x.rs2 x.imm x.rs1
        | C_SRLI x | C_SRAI x | C_SLLI x -> sprintf "x%d, %d" x.rd x.shamt
        | C_SUB x | C_XOR x | C_OR x | C_AND x | C_SUBW x | C_ADDW x | C_MV x | C_ADD x -> sprintf "x%d, x%d" x.rd x.rs2
        | C_J x | C_JAL x | C_ADDI16SP x -> sprintf "%d" x.imm
        | C_BEQZ x | C_BNEZ x -> sprintf "x%d, %d" x.rs1 x.imm
        | C_JR x | C_JALR x -> sprintf "x%d" x.rs1
        | C_SWSP x | C_SDSP x -> sprintf "x%d, %d(x2)" x.rs2 x.imm
        | _ -> "Undef"
    let pc = sprintf "%08x:" mstate.PC
    let instr = sprintf "%04x" instr
    let instrMsg = String.Format("{0,-7}{1}", typeName, instrMsg)
    printfn "%s" (String.Format("{0,-12}{1,-12}{2}", pc, instr, instrMsg))
