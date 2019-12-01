module ISA.RISCV.Execute.M

open ISA.RISCV.Decode.M
open ISA.RISCV.Arch
open ISA.RISCV.MachineState
open ISA.RISCV.Utils.Bits

//=================================================
// MUL - Multiplication operation - sign * sign
let execMUL (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let rdVal = (mstate.getRegister rs1) * (mstate.getRegister rs2)
    let mstate = mstate.setRegister rd rdVal
    mstate.incPC

//=================================================
// MULH - Multiplication operation - sign * sign and return high 32 bits
let execMULH (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let rdVal = (mstate.getRegister rs1) * (mstate.getRegister rs2)
    let hRes =
        match mstate.Arch.archBits with
        | RV32 -> rdVal.bitSlice 63 32
        | _    -> 0L
    let mstate = mstate.setRegister rd hRes
    mstate.incPC

//=================================================
// MULHSU - Multiplication operation - Multiplication operation - sign * unsign and return high 32 bits
let execMULHSU (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let rs1Val = mstate.getRegister rs1
    let rs2Val = mstate.getRegister rs2
    let rdVal =
        match mstate.Arch.archBits with
        | RV32 -> rs1Val * int64(uint32 rs2Val)
        | _    -> rs1Val * int64(uint64 rs2Val)
    let hRes = rdVal.bitSlice 63 32
    let mstate = mstate.setRegister rd hRes
    mstate.incPC

//=================================================
// MULHU - Multiplication operation - Multiplication operation - unsign * unsign and return high 32 bits
let execMULHU (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let rs1Val = mstate.getRegister rs1
    let rs2Val = mstate.getRegister rs2
    let rdVal =
        match mstate.Arch.archBits with
        | RV32 -> int64(uint32 rs1Val) * int64(uint32 rs2Val)
        | _    -> int64(uint64 rs1Val) * int64(uint64 rs2Val)
    let hRes = rdVal.bitSlice 63 32
    let mstate = mstate.setRegister rd hRes
    mstate.incPC

//=================================================
// DIV - Division operation
let execDIV (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let rs1Val = mstate.getRegister rs1
    let rs2Val = mstate.getRegister rs2
    let minSigned =
        match mstate.Arch.archBits with
        | RV32 -> 0x80000000L
        | _    -> 0x8000000000000000L
    let rdVal =
        if rs2Val = 0L then
            -1L
        else if rs1Val = minSigned && rs2Val = -1L then
            rs1Val
        else
            rs1Val / rs2Val
    let mstate = mstate.setRegister rd rdVal
    mstate.incPC

//=================================================
// DIVU - Division unsign operation
let execDIVU (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let rs1Val = mstate.getRegister rs1
    let rs2Val = mstate.getRegister rs2
    let rdVal =
        match mstate.Arch.archBits with
        | RV32 ->
            let maxUnsigned = 0xFFFFFFFFL
            if rs2Val = 0L then
                maxUnsigned
            else
                int64(uint32 rs1Val / uint32 rs2Val)
        | _    ->
            let maxUnsigned = 0xFFFFFFFFFFFFFFFFL
            if rs2Val = 0L then
                maxUnsigned
            else
                int64(uint64 rs1Val / uint64 rs2Val)
    let mstate = mstate.setRegister rd rdVal
    mstate.incPC

//=================================================
// REM - Rem operation
let execREM (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let rs1Val = mstate.getRegister rs1
    let rs2Val = mstate.getRegister rs2
    let minSigned =
        match mstate.Arch.archBits with
        | RV32 -> 0x80000000L
        | _    -> 0x8000000000000000L
    let rdVal =
        if rs2Val = 0L then
            rs1Val
        else if rs1Val = minSigned && rs2Val = -1L then
            0L
        else
            rs1Val % rs2Val
    let mstate = mstate.setRegister rd rdVal
    mstate.incPC

//=================================================
// REMU - Rem unsign operation
let execREMU (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let rs1Val = mstate.getRegister rs1
    let rs2Val = mstate.getRegister rs2
    let rdVal =
        match mstate.Arch.archBits with
        | RV32 ->
            if rs2Val = 0L then
                rs1Val
            else
                int64(uint32 rs1Val % uint32 rs2Val)
        | _    ->
            if rs2Val = 0L then
                rs1Val
            else
                int64(uint64 rs1Val % uint64 rs2Val)
    let mstate = mstate.setRegister rd rdVal
    mstate.incPC

// Execute M-instructions
let Execute (instr : InstructionM) (mstate : MachineState) =
    match instr with
    | MUL i ->
        execMUL i.rd i.rs1 i.rs2 mstate
    | MULH i ->
        execMULH i.rd i.rs1 i.rs2 mstate
    | MULHSU i ->
        execMULHSU i.rd i.rs1 i.rs2 mstate
    | MULHU i ->
        execMULHU i.rd i.rs1 i.rs2 mstate
    | DIV i ->
        execDIV i.rd i.rs1 i.rs2 mstate
    | DIVU i ->
        execDIVU i.rd i.rs1 i.rs2 mstate
    | REM i ->
        execREM i.rd i.rs1 i.rs2 mstate
    | REMU i ->
        execREMU i.rd i.rs1 i.rs2 mstate
    | _ -> mstate.setRunState (Trap InstructionExecute)
