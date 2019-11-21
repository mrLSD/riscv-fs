module ISA.RISCV.Execute.M64

open ISA.RISCV.Arch
open ISA.RISCV.Decode.M
open ISA.RISCV.MachineState

//=================================================
// MULW - Multiplication Word operation - sign * sign
let execMULW (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let rdVal = int32(mstate.getRegister rs1) * int32(mstate.getRegister rs2)
    let mstate = mstate.setRegister rd (int64 rdVal)
    mstate.incPC


//=================================================
// DIVW - Division Word operation - sign * sign
let execDIVW (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let rs1Val = mstate.getRegister rs1
    let rs2Val = mstate.getRegister rs2
    let minSigned = 0x8000000000000000L
    let rdVal =
        if rs2Val = 0L then
            -1
        else if rs1Val = minSigned && rs2Val = -1L then
            int32 rs1Val
        else
            int32 rs1Val / int32 rs2Val
    let mstate = mstate.setRegister rd (int64 rdVal)
    mstate.incPC

//=================================================
// DIVUW - Division Unsign Word operation - sign * sign
let execDIVUW (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let rs1Val = mstate.getRegister rs1
    let rs2Val = mstate.getRegister rs2
    let maxUnsigned = 0xFFFFFFFFFFFFFFFFL
    let rdVal =
        if rs2Val = 0L then
            int32 maxUnsigned
        else
            int32(uint64 rs1Val / uint64 rs2Val)
    let mstate = mstate.setRegister rd (int64 rdVal)
    mstate.incPC

//=================================================
// REMW - Division Unsign Word operation - sign * sign
let execREMW (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let rs1Val = mstate.getRegister rs1
    let rs2Val = mstate.getRegister rs2
    let minSigned = 0x8000000000000000L
    let rdVal =
        if rs2Val = 0L then
            int32 rs1Val
        else if rs1Val = minSigned && rs2Val = -1L then
            0
        else
            int32  rs1Val % int32 rs2Val
    let mstate = mstate.setRegister rd (int64 rdVal)
    mstate.incPC

//=================================================
// REMUW - Division Unsign Word operation - sign * sign
let execREMUW (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let rs1Val = mstate.getRegister rs1
    let rs2Val = mstate.getRegister rs2
    let rdVal =
        if rs2Val = 0L then
            int32 rs1Val
        else
            int32(uint64 rs1Val % uint64 rs2Val)
    let mstate = mstate.setRegister rd (int64 rdVal)
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
