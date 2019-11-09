module ISA.RISCV.Execute.M

open ISA.RISCV.Arch
open ISA.RISCV.Decode
open ISA.RISCV.MachineState
open ISA.RISCV.Utils.Bits

// Execute M-instructions
let Execute (instr : M.InstructionM) (mstate : MachineState) =
    match instr with
    | _ -> mstate.setRunState (Trap InstructionExecute)
