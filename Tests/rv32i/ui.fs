module Tests.rv32i.ui

open Xunit

open ISA.RISCV
open ISA.RISCV.Arch
open ISA.RISCV.Decode

//===============================================
// Upper immediate tests
let Ui instr imm =
    // Init MachineState
    let addr = 0x80000000L
    let mstate = MachineState.InitMachineState Map.empty RV32i true
    let mstate = mstate.setPC addr

    let decodedInstr = I.DecodeI instr
    Assert.NotEqual(decodedInstr, I.None)
    let mstate = ExecuteI.ExecuteI decodedInstr mstate
    mstate.incPC
