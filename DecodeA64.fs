module ISA.RISCV.Decode.A64

open System
open ISA.RISCV.Utils.Bits
open ISA.RISCV.Arch
open ISA.RISCV.MachineState

//================================================================
// 'A64'  (Atomic Memory Operations 'A' Standard Extension)
type InstructionA64 =
    | LR_D   of  {| rd: Register; rs1: Register; aq: InstrField; rl: InstrField |}
    | SC_D   of  {| rd: Register; rs1: Register; rs2: Register; aq: InstrField; rl: InstrField |}

    | AMOSWAP_D  of  {| rd: Register; rs1: Register; rs2: Register; aq: InstrField; rl: InstrField |}
    | AMOADD_D   of  {| rd: Register; rs1: Register; rs2: Register; aq: InstrField; rl: InstrField |}
    | AMOXOR_D   of  {| rd: Register; rs1: Register; rs2: Register; aq: InstrField; rl: InstrField |}
    | AMOAND_D   of  {| rd: Register; rs1: Register; rs2: Register; aq: InstrField; rl: InstrField |}
    | AMOOR_D    of  {| rd: Register; rs1: Register; rs2: Register; aq: InstrField; rl: InstrField |}
    | AMOMIN_D   of  {| rd: Register; rs1: Register; rs2: Register; aq: InstrField; rl: InstrField |}
    | AMOMAX_D   of  {| rd: Register; rs1: Register; rs2: Register; aq: InstrField; rl: InstrField |}
    | AMOMINU_D  of  {| rd: Register; rs1: Register; rs2: Register; aq: InstrField; rl: InstrField |}
    | AMOMAXU_D  of  {| rd: Register; rs1: Register; rs2: Register; aq: InstrField; rl: InstrField |}
    
    | None // Instruction not found

/// Decode 'A64' instructions
let Decode (instr: InstrField) : InstructionA64 =
    let opcode = instr.bitSlice 6 0
    // Register number can be: 0-32
    let rd     = int32(instr.bitSlice 11  7)
    let rs1    = int32(instr.bitSlice 19 15)
    let rs2    = int32(instr.bitSlice 24 20)
    
    let funct3 = instr.bitSlice 14 12
    let funct7 = instr.bitSlice 31 27
    
    let rl = instr.bitSlice 25 25
    let aq = instr.bitSlice 26 26
    
    match (opcode) with
    | 0b0101111 when funct3 = 0b011 ->
        match funct7 with
        | 00010 -> LR_D      {| rd = rd; rs1 = rs1; aq = aq; rl = rl |}
        | 00011 -> SC_D      {| rd = rd; rs1 = rs1; rs2 = rs1; aq = aq; rl = rl |}  
        | 00001 -> AMOSWAP_D {| rd = rd; rs1 = rs1; rs2 = rs1; aq = aq; rl = rl |}
        | 00000 -> AMOADD_D  {| rd = rd; rs1 = rs1; rs2 = rs1; aq = aq; rl = rl |} 
        | 00100 -> AMOXOR_D  {| rd = rd; rs1 = rs1; rs2 = rs1; aq = aq; rl = rl |}
        | 01100 -> AMOAND_D  {| rd = rd; rs1 = rs1; rs2 = rs1; aq = aq; rl = rl |}
        | 01000 -> AMOOR_D   {| rd = rd; rs1 = rs1; rs2 = rs1; aq = aq; rl = rl |}
        | 10000 -> AMOMIN_D  {| rd = rd; rs1 = rs1; rs2 = rs1; aq = aq; rl = rl |}
        | 10100 -> AMOMAX_D  {| rd = rd; rs1 = rs1; rs2 = rs1; aq = aq; rl = rl |}
        | 11000 -> AMOMINU_D {| rd = rd; rs1 = rs1; rs2 = rs1; aq = aq; rl = rl |}
        | 11100 -> AMOMAXU_D {| rd = rd; rs1 = rs1; rs2 = rs1; aq = aq; rl = rl |}
        | _ -> None
    
    | _ -> None

let verbosityMessage (instr : InstrField) (decodedInstr : InstructionA64) (mstate : MachineState) =
    let typeName = decodedInstr.GetType().Name
    let instrMsg =
        match (decodedInstr) with
        | LR_D x -> sprintf "x%d, x%d" x.rd x.rs1    
        | SC_D x | AMOSWAP_D x | AMOADD_D x | AMOXOR_D x
        | AMOAND_D x | AMOOR_D x | AMOMIN_D x
        | AMOMAX_D x | AMOMINU_D x | AMOMAXU_D x-> sprintf "x%d, x%d, x%d" x.rd x.rs1 x.rs2
        | _ -> "Undef"
    let pc = sprintf "%08x:" mstate.PC
    let instr = sprintf "%08x" instr
    let instrMsg = String.Format("{0,-7}{1}", typeName, instrMsg)
    printfn "%s" (String.Format("{0,-12}{1,-12}{2}", pc, instr, instrMsg))
