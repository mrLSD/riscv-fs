module ISA.RISCV.Execute.M

open ISA.RISCV.Decode.M
open ISA.RISCV.Arch
open ISA.RISCV.MachineState

//=================================================
// MUL - Multiplication operation - sign * sign
let execMUL (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    // mul  = s*s
    mstate.incPC

//=================================================
// MULH - Multiplication operation - sign * sign and return high 32 bits
let execMULH (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    // mulh = s*s -> [63..32]
    mstate.incPC

//=================================================
// MULHSU - Multiplication operation - Multiplication operation - sign * unsign and return high 32 bits
let execMULHSU (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    // mulhsu = s*us(32|64) -> [63..32]
    mstate.incPC

//=================================================
// MULHU - Multiplication operation - Multiplication operation - unsign * unsign and return high 32 bits
let execMULHU (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    // mulhu = us(32|64)*us(32|64) -> [63..32]
    mstate.incPC

//=================================================
// DIV - Division operation
let execDIV (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    mstate.incPC

//=================================================
// DIVU - Division unsign operation
let execDIVU (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    mstate.incPC

//=================================================
// REM - Rem operation
let execREM (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    mstate.incPC

//=================================================
// REMU - Rem unsign operation
let execREMU (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
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
