module ISA.RISCV.Fetch

open ISA.RISCV.MachineState
open ISA.RISCV.Utils.Bits
open ISA.RISCV.Arch

let FetchInstruction (binData : byte array) (mstate : MachineState) : InstrField =
    loadWord binData mstate.PC
