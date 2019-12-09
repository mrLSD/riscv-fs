module ISA.RISCV.Decode.M

open System
open ISA.RISCV.Utils.Bits
open ISA.RISCV.Arch
open ISA.RISCV.MachineState

//================================================================ -- \begin_latex{Major_Opcodes}
// 'M'  (Integer Multiplication and Division 'M' Standard Extension)
type InstructionM =
    | MUL    of  {| rd: Register; rs1: Register; rs2: Register |}
    | MULH   of  {| rd: Register; rs1: Register; rs2: Register |}
    | MULHSU of  {| rd: Register; rs1: Register; rs2: Register |}
    | MULHU  of  {| rd: Register; rs1: Register; rs2: Register |}
    | DIV    of  {| rd: Register; rs1: Register; rs2: Register |}
    | DIVU   of  {| rd: Register; rs1: Register; rs2: Register |}
    | REM    of  {| rd: Register; rs1: Register; rs2: Register |}
    | REMU   of  {| rd: Register; rs1: Register; rs2: Register |}

    | None // Instruction not found

/// Decode 'M' instructions
let Decode (mstate : MachineState) (instr: InstrField) : InstructionM =
    let opcode = instr.bitSlice 6 0
    // Register number can be: 0-32
    let rd     = int32(instr.bitSlice 11  7)
    let rs1    = int32(instr.bitSlice 19 15)
    let rs2    = int32(instr.bitSlice 24 20)

    let funct3 = instr.bitSlice 14 12
    let funct7 = instr.bitSlice 31 25

    match (opcode) with
    // RV32M Standard Extension
    | 0b0110011 ->
        match funct7 with
        | 0b0000001 ->
            match funct3 with
            // Multiplication Operations
            | 0b000 -> MUL    {| rd = rd; rs1 = rs1; rs2 = rs2 |}
            | 0b001 -> MULH   {| rd = rd; rs1 = rs1; rs2 = rs2 |}
            | 0b010 -> MULHSU {| rd = rd; rs1 = rs1; rs2 = rs2 |}
            | 0b011 -> MULHU  {| rd = rd; rs1 = rs1; rs2 = rs2 |}
            // Division Operations
            | 0b100 -> DIV    {| rd = rd; rs1 = rs1; rs2 = rs2 |}
            | 0b101 -> DIVU   {| rd = rd; rs1 = rs1; rs2 = rs2 |}
            | 0b110 -> REM    {| rd = rd; rs1 = rs1; rs2 = rs2 |}
            | 0b111 -> REMU   {| rd = rd; rs1 = rs1; rs2 = rs2 |}
            | _     -> None
        | _ -> None

    | _ -> None

// Current ISA print log message for current instruction step
let verbosityMessage (instr : InstrField) (decodedInstr : InstructionM) (mstate : MachineState) =
    let typeName = decodedInstr.GetType().Name
    let instrMsg =
        match (decodedInstr) with
        | MUL x   | MULH x | MULHSU x | MULHU x | DIV x
        | DIVU x  | REM x  | REMU x   -> sprintf "x%d, x%d, x%d" x.rd x.rs1 x.rs2
        | _ -> "Undef"
    let pc = sprintf "%08x:" mstate.PC
    let instr = sprintf "%08x" instr
    let instrMsg = String.Format("{0,-7}{1}", typeName, instrMsg)
    printfn "%s" (String.Format("{0,-12}{1,-12}{2}", pc, instr, instrMsg))
