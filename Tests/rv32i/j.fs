module Tests.rv32i.j

open Xunit

open ISA.RISCV
open ISA.RISCV.Arch
open ISA.RISCV.Decode

//===============================================
// Jump tests
let Jump instr x2 x3 resultAddr =
    // Init MachineState
    let addr = 0x80000000L
    let mstate = MachineState.InitMachineState Map.empty RV32i true
    let mstate = mstate.setPC addr
    let mstate = mstate.setRegister 2 x2
    let resMstate = mstate.incPC

    let decodedInstr = I.DecodeI instr
    Assert.NotEqual(decodedInstr, I.None)
    let mstate = ExecuteI.ExecuteI decodedInstr mstate
    Assert.Equal(x2, mstate.getRegister 2)
    Assert.Equal(resMstate.PC, mstate.getRegister 3)
    Assert.Equal(resultAddr, mstate.PC)

[<Theory>]
[<InlineData(0x018001ef, 0x80000004L)>]
[<InlineData(0xff5ff1ef, 0x80000004L)>]
let ``JAL: x3, addr`` (instr, addrRes) =
    Jump instr 0L 3L addrRes
