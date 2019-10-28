module ISA.RISCV.Execute.I64

open ISA.RISCV.Decode.I64
open ISA.RISCV.MachineState

// Execute I-instructions
let Execute (instr : InstructionI64) (mstate : MachineState) =
    mstate
