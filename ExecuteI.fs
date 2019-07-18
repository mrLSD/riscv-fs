module ISA.RISCV.ExecuteI

open ISA.RISCV.Arch
open ISA.RISCV.Decode.I
open ISA.RISCV.MachineState
open ISA.RISCV.Utils.Bits

//=================================================
// LUI
let execLUI (rd : Register) (imm20 : MachineInt) (mstate : MachineState) =
    let mstate = mstate.setRegister rd imm20
    mstate.incPC

//=================================================
// AUIPC
let execAUIPC (rd : Register) (imm20 : MachineInt) (mstate : MachineState) =
    let mstate = mstate.setRegister rd (imm20 + mstate.PC)
    mstate.incPC

//=================================================
// JALR
let execJALR (rd : Register) (rs1 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    let newPC = (int64(mstate.getRegister rs1) + imm12) &&& (~~~1L)
    if newPC % 4L <> 0L then
        mstate.setRunState (Trap JumpAddress)
    else
        let mstate = mstate.setRegister rd (mstate.PC + 4L)
        mstate.setPC (int64(newPC))

//=================================================
// JAL
let execJAL (rd : Register) (imm20 : MachineInt) (mstate : MachineState) =
    let newPC = mstate.PC + int64(imm20)
    if newPC % 4L <> 0L then
        mstate.setRunState (Trap JumpAddress)
    else
        let mstate = mstate.setRegister rd (mstate.PC + 4L)
        mstate.setPC newPC

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

//=================================================
// LB
let execLB (rd : Register) (rs1 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    mstate

//=================================================
// LH
let execLH (rd : Register) (rs1 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    mstate

//=================================================
// LW
let execLW (rd : Register) (rs1 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    mstate

//=================================================
// LBU
let execLBU (rd : Register) (rs1 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    mstate

//=================================================
// LHU
let execLHU (rd : Register) (rs1 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    mstate

//=================================================
// SB
let execSB (rs1 : Register) (rs2 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    mstate

//=================================================
// SH
let execSH (rs1 : Register) (rs2 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    mstate

//=================================================
// SW
let execSW (rs1 : Register) (rs2 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    mstate

//=================================================
// ADDI
let execADDI (rd : Register) (rs1 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    mstate

//=================================================
// SLTI
let execSLTI (rd : Register) (rs1 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    mstate

//=================================================
// SLTIU
let execSLTIU (rd : Register) (rs1 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    mstate

//=================================================
// XORI
let execXORI (rd : Register) (rs1 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    mstate

//=================================================
// ORI
let execORI (rd : Register) (rs1 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    mstate

//=================================================
// SLLI
let execSLLI (rd : Register) (rs1 : Register) (shamt : MachineInt) (mstate : MachineState) =
    mstate

//=================================================
// SRLI
let execSRLI (rd : Register) (rs1 : Register) (shamt : MachineInt) (mstate : MachineState) =
    mstate

//=================================================
// SRAI
let execSRAI (rd : Register) (rs1 : Register) (shamt : MachineInt) (mstate : MachineState) =
    mstate

//=================================================
// ANDI
let execANDI (rd : Register) (rs1 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    mstate

//=================================================
// ADD
let execADD (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    mstate

//=================================================
// SUB
let execSUB (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    mstate

//=================================================
// SLL
let execSLL (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    mstate

//=================================================
// SLT
let execSLT (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    mstate

//=================================================
// SLTU
let execSLTU (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    mstate

//=================================================
// XOR
let execXOR (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    mstate

//=================================================
// SRL
let execSRL (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    mstate

//=================================================
// SRA
let execSRA (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    mstate

//=================================================
// OR
let execOR (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    mstate

//=================================================
// AND
let execAND (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
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
    | LB i ->
        execLB i.rd i.rs1 i.imm12 mstate
    | LH i ->
        execLH i.rd i.rs1 i.imm12 mstate
    | LW i ->
        execLW i.rd i.rs1 i.imm12 mstate
    | LBU i ->
        execLBU i.rd i.rs1 i.imm12 mstate
    | LHU i ->
        execLHU i.rd i.rs1 i.imm12 mstate
    | SB i ->
        execSB i.rs1 i.rs2 i.imm12 mstate
    | SH i ->
        execSH i.rs1 i.rs2 i.imm12 mstate
    | SW i ->
        execSW i.rs1 i.rs2 i.imm12 mstate
    | ADDI i ->
        execADDI i.rd i.rs1 i.imm12 mstate
    | SLTI i ->
        execSLTI i.rd i.rs1 i.imm12 mstate
    | SLTIU i ->
        execSLTIU i.rd i.rs1 i.imm12 mstate
    | XORI i ->
        execXORI i.rd i.rs1 i.imm12 mstate
    | ORI i ->
        execORI i.rd i.rs1 i.imm12 mstate
    | ANDI i ->
        execANDI i.rd i.rs1 i.imm12 mstate
    | SLLI i ->
        execSLLI i.rd i.rs1 i.shamt mstate
    | SRLI i ->
        execSRLI i.rd i.rs1 i.shamt mstate
    | SRAI i ->
        execSRAI i.rd i.rs1 i.shamt mstate
    | ADD i ->
        execADD i.rd i.rs1 i.rs2 mstate
    | SUB i ->
        execSUB i.rd i.rs1 i.rs2 mstate
    | SLL i ->
        execSLL i.rd i.rs1 i.rs2 mstate
    | SLT i ->
        execSLT i.rd i.rs1 i.rs2 mstate
    | SLTU i ->
        execSLTU i.rd i.rs1 i.rs2 mstate
    | XOR i ->
        execXOR i.rd i.rs1 i.rs2 mstate
    | SRL i ->
        execSRL i.rd i.rs1 i.rs2 mstate
    | SRA i ->
        execSRA i.rd i.rs1 i.rs2 mstate
    | OR i ->
        execOR i.rd i.rs1 i.rs2 mstate
    | AND i ->
        execAND i.rd i.rs1 i.rs2 mstate
    | _ -> mstate
