module ISA.RISCV.Fetch

open ISA.RISCV.MachineState
open ISA.RISCV.Arch

let FetchInstruction (mstate : MachineState) : InstrField = 0