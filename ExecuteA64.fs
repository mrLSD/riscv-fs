module ISA.RISCV.Execute.A64

open ISA.RISCV.Arch
open ISA.RISCV.Decode.A64
open ISA.RISCV.MachineState

//=================================================
// LR.D - Load-Reserved Double Word operation
let execLR_D (rd : Register) (rs1 : Register) (mstate : MachineState) =
    mstate.incPC

// Execute A-instructions
let Execute (instr : InstructionA64) (mstate : MachineState) =
    match instr with
    | LR_D i ->
        execLR_D i.rd i.rs1 mstate
    | _ -> mstate.setRunState (Trap InstructionExecute)
