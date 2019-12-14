module ISA.RISCV.Decode.M64

open System
open ISA.RISCV.Utils.Bits
open ISA.RISCV.Arch
open ISA.RISCV.MachineState

//================================================================
// 'M64'  (Integer Multiplication and Division 'M' Standard Extension)
type InstructionM64 =
    | MULW   of  {| rd: Register; rs1: Register; rs2: Register |}
    | DIVW   of  {| rd: Register; rs1: Register; rs2: Register |}
    | DIVUW  of  {| rd: Register; rs1: Register; rs2: Register |}
    | REMW   of  {| rd: Register; rs1: Register; rs2: Register |}
    | REMUW  of  {| rd: Register; rs1: Register; rs2: Register |}

    | None // Instruction not found

/// Decode 'M64' instructions
let Decode (mstate : MachineState) (instr: InstrField) : InstructionM64 =
    let opcode = instr.bitSlice 6 0
    // Register number can be: 0-32
    let rd     = int32(instr.bitSlice 11  7)
    let rs1    = int32(instr.bitSlice 19 15)
    let rs2    = int32(instr.bitSlice 24 20)

    let funct3 = instr.bitSlice 14 12
    let funct7 = instr.bitSlice 31 25

    match (opcode) with
    // RV64M Standard Extension (in addition to RV32M)
    | 0b0111011 when mstate.Arch.archBits = RV64 ->
        match funct7 with
        | 0b0000001 ->
            match funct3 with
            // Multiplication Operations
            | 0b000 -> MULW   {| rd = rd; rs1 = rs1; rs2 = rs2 |}
            // Division Operations
            | 0b100 -> DIVW   {| rd = rd; rs1 = rs1; rs2 = rs2 |}
            | 0b101 -> DIVUW  {| rd = rd; rs1 = rs1; rs2 = rs2 |}
            | 0b110 -> REMW   {| rd = rd; rs1 = rs1; rs2 = rs2 |}
            | 0b111 -> REMUW  {| rd = rd; rs1 = rs1; rs2 = rs2 |}
            | _     -> None
        | _ -> None

    | _ -> None

// Current ISA print log message for current instruction step
let verbosityMessage (instr : InstrField) (decodedInstr : InstructionM64) (mstate : MachineState) =
    let typeName = decodedInstr.GetType().Name
    let instrMsg =
        match (decodedInstr) with
        MULW x  | DIVW x | DIVUW x |
        REMW x  | REMUW x -> sprintf "x%d, x%d, x%d" x.rd x.rs1 x.rs2
        | _ -> "Undef"
    let pc = sprintf "%08x:" mstate.PC
    let instr = sprintf "%08x" instr
    let instrMsg = String.Format("{0,-7}{1}", typeName, instrMsg)
    printfn "%s" (String.Format("{0,-12}{1,-12}{2}", pc, instr, instrMsg))
