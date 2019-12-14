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

    | None // Instruction not found