module ISA.RISCV.ExecuteI

open ISA.RISCV.Arch
open ISA.RISCV.Decode.I
open ISA.RISCV.MachineState
open ISA.RISCV.Utils.Bits

//---------------------------------------
// LUI
let execLUI (rd : Register) (imm20 : MachineInt) (mstate : MachineState) =
    let imm = (imm20 <<< 12).signExtend 32
    let mstate = mstate.setRegister rd imm
    mstate.incPC

let ExecuteI (instr : InstructionI) (mstate : MachineState) =
    match instr with
    | LUI i ->
        execLUI i.rd i.imm20 mstate
    | _ -> mstate
