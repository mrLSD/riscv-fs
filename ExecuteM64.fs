module ISA.RISCV.Execute.M64

open ISA.RISCV.Arch
open ISA.RISCV.Decode.M
open ISA.RISCV.MachineState

//=================================================
// MULW - Multiplication Word operation - sign * sign
let execMULW (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    // mulw = 32(s)*32(s) -> sign_ext
    mstate.incPC

//=================================================
// DIVW - Division Word operation - sign * sign
let execDIVW (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    mstate.incPC

//=================================================
// DIVUW - Division Unsign Word operation - sign * sign
let execDIVUW (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    mstate.incPC

//=================================================
// REMW - Division Unsign Word operation - sign * sign
let execREMW (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    mstate.incPC

//=================================================
// REMUW - Division Unsign Word operation - sign * sign
let execREMUW (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    mstate.incPC

// Execute M64-instructions
let Execute (instr : InstructionM) (mstate : MachineState) =
    match instr with
    | MULW i ->
        execMULW i.rd i.rs1 i.rs2 mstate
    | DIVW i ->
        execDIVW i.rd i.rs1 i.rs2 mstate
    | DIVUW i ->
        execDIVUW i.rd i.rs1 i.rs2 mstate
    | REMW i ->
        execREMW i.rd i.rs1 i.rs2 mstate
    | REMUW i ->
        execREMUW i.rd i.rs1 i.rs2 mstate
    | _ -> mstate.setRunState (Trap InstructionExecute)
