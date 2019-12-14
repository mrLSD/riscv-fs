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

    | None // Instruction not found