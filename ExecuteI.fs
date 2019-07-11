module ISA.RISCV.ExecuteI

open ISA.RISCV.Arch
open ISA.RISCV.Decode.I
open ISA.RISCV.MachineState
open ISA.RISCV.Utils.Bits

//=================================================
// LUI
let execLUI (rd : Register) (imm20 : MachineInt) (mstate : MachineState) =
    let imm = (imm20 <<< 12).signExtend 32
    let mstate = mstate.setRegister rd imm
    mstate.incPC

//=================================================
// AUIPC
let execAUIPC (rd : Register) (imm20 : MachineInt) (mstate : MachineState) =
    mstate

//=================================================
// JALR
let execJALR (rd : Register) (rs1 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    mstate

//=================================================
// JAL
let execJAL (rd : Register) (imm20 : MachineInt) (mstate : MachineState) =
    mstate

//=================================================
// BEQ
let execBEQ (rs1 : Register) (rs2 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    mstate

//=================================================
// BNE
let execBNE (rs1 : Register) (rs2 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    mstate

//=================================================
// BLT
let execBLT (rs1 : Register) (rs2 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    mstate

//=================================================
// BGE
let execBGE (rs1 : Register) (rs2 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    mstate

//=================================================
// BLTU
let execBLTU (rs1 : Register) (rs2 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    mstate

//=================================================
// BGEU
let execBGEU (rs1 : Register) (rs2 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    mstate

// Execute I-instructions
let ExecuteI (instr : InstructionI) (mstate : MachineState) =
    match instr with
    | LUI i ->
        execLUI i.rd i.imm20 mstate
    | AUIPC i ->
        execAUIPC i.rd i.imm20 mstate
    | JALR i ->
        execJALR i.rd i.rs1 i.imm12 mstate
    | JAL i ->
        execJAL i.rd i.imm20 mstate
    | BEQ i ->
        execBEQ i.rs1 i.rs2 i.imm12 mstate
    | BNE i ->
        execBNE i.rs1 i.rs2 i.imm12 mstate
    | BLT i ->
        execBLT i.rs1 i.rs2 i.imm12 mstate
    | BGE i ->
        execBGE i.rs1 i.rs2 i.imm12 mstate
    | BLTU i ->
        execBLTU i.rs1 i.rs2 i.imm12 mstate
    | BGEU i ->
        execBGEU i.rs1 i.rs2 i.imm12 mstate
    | _ -> mstate
