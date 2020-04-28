module ISA.RISCV.Decode.A

open System
open ISA.RISCV.Utils.Bits
open ISA.RISCV.Arch
open ISA.RISCV.MachineState

//================================================================
// 'A32'  (Atomic Memory Operations 'A' Standard Extension)
type InstructionA =
    | LR_W   of  {| rd: Register; rs1: Register; aq: InstrField; rl: InstrField |}
    | SC_W   of  {| rd: Register; rs1: Register; rs2: Register; aq: InstrField; rl: InstrField |}

    | AMOSWAP_W  of  {| rd: Register; rs1: Register; rs2: Register; aq: InstrField; rl: InstrField |}
    | AMOADD_W   of  {| rd: Register; rs1: Register; rs2: Register; aq: InstrField; rl: InstrField |}
    | AMOXOR_W   of  {| rd: Register; rs1: Register; rs2: Register; aq: InstrField; rl: InstrField |}
    | AMOAND_W   of  {| rd: Register; rs1: Register; rs2: Register; aq: InstrField; rl: InstrField |}
    | AMOOR_W    of  {| rd: Register; rs1: Register; rs2: Register; aq: InstrField; rl: InstrField |}
    | AMOMIN_W   of  {| rd: Register; rs1: Register; rs2: Register; aq: InstrField; rl: InstrField |}
    | AMOMAX_W   of  {| rd: Register; rs1: Register; rs2: Register; aq: InstrField; rl: InstrField |}
    | AMOMINU_W  of  {| rd: Register; rs1: Register; rs2: Register; aq: InstrField; rl: InstrField |}
    | AMOMAXU_W  of  {| rd: Register; rs1: Register; rs2: Register; aq: InstrField; rl: InstrField |}
    
    | None // Instruction not found

/// Decode 'A' instructions
let Decode (instr: InstrField) : InstructionA =
    let opcode = instr.bitSlice 6 0
    // Register number can be: 0-32
    let rd     = int32(instr.bitSlice 11  7)
    let rs1    = int32(instr.bitSlice 19 15)
    let rs2    = int32(instr.bitSlice 24 20)
    
    let funct3 = instr.bitSlice 14 12
    let funct7 = instr.bitSlice 31 27
    
    let rl = instr.bitSlice 25 25
    let aq = instr.bitSlice 26 26
    
    printfn "%X[%X] %X" opcode 0b0101111 funct3
    
    match (opcode) with
    | 0b0101111 when funct3 = 0b010 ->
        match funct7 with
        | 00010 -> LR_W      {| rd = rd; rs1 = rs1; aq = aq; rl = rl |}
        | 00011 -> SC_W      {| rd = rd; rs1 = rs1; rs2 = rs2; aq = aq; rl = rl |}  
        | 00001 -> AMOSWAP_W {| rd = rd; rs1 = rs1; rs2 = rs2; aq = aq; rl = rl |}
        | 00000 -> AMOADD_W  {| rd = rd; rs1 = rs1; rs2 = rs2; aq = aq; rl = rl |} 
        | 00100 -> AMOXOR_W  {| rd = rd; rs1 = rs1; rs2 = rs2; aq = aq; rl = rl |}
        | 01100 -> AMOAND_W  {| rd = rd; rs1 = rs1; rs2 = rs2; aq = aq; rl = rl |}
        | 01000 -> AMOOR_W   {| rd = rd; rs1 = rs1; rs2 = rs2; aq = aq; rl = rl |}
        | 10000 -> AMOMIN_W  {| rd = rd; rs1 = rs1; rs2 = rs2; aq = aq; rl = rl |}
        | 10100 -> AMOMAX_W  {| rd = rd; rs1 = rs1; rs2 = rs2; aq = aq; rl = rl |}
        | 11000 -> AMOMINU_W {| rd = rd; rs1 = rs1; rs2 = rs2; aq = aq; rl = rl |}
        | 11100 -> AMOMAXU_W {| rd = rd; rs1 = rs1; rs2 = rs2; aq = aq; rl = rl |}
        | _ -> None
    
    | _ -> None

let verbosityMessage (instr : InstrField) (decodedInstr : InstructionA) (mstate : MachineState) =
    let typeName = decodedInstr.GetType().Name
    let instrMsg =
        match (decodedInstr) with
        | LR_W x -> sprintf "x%d, x%d" x.rd x.rs1    
        | SC_W x | AMOSWAP_W x | AMOADD_W x | AMOXOR_W x
        | AMOAND_W x | AMOOR_W x | AMOMIN_W x
        | AMOMAX_W x | AMOMINU_W x | AMOMAXU_W x-> sprintf "x%d, x%d, x%d" x.rd x.rs1 x.rs2
        | _ -> "Undef"
    let pc = sprintf "%08x:" mstate.PC
    let instr = sprintf "%08x" instr
    let instrMsg = String.Format("{0,-7}{1}", typeName, instrMsg)
    printfn "%s" (String.Format("{0,-12}{1,-12}{2}", pc, instr, instrMsg))
