module ISA.RISCV.Fetch

open ISA.RISCV.MachineState
open ISA.RISCV.Utils.Bits
open ISA.RISCV.Arch

let FetchInstruction (mstate : MachineState) : InstrField =
    loadWord mstate.Memory mstate.PC
