module ISA.RISCV.Execute.A

open ISA.RISCV.Arch
open ISA.RISCV.Decode.A
open ISA.RISCV.MachineState

//=================================================
// LR.W - Load-Reserved Word operation
let execLR_W (rd : Register) (rs1 : Register) (mstate : MachineState) =
    mstate.incPC

// Execute A-instructions
let Execute (instr : InstructionA) (mstate : MachineState) =
    match instr with
    | LR_W i ->
        execLR_W i.rd i.rs1 mstate
    | _ -> mstate.setRunState (Trap InstructionExecute)
