module Tests.rv32i.br

open Xunit

open ISA.RISCV
open ISA.RISCV.Arch
open ISA.RISCV.Decode

//===============================================
// Branch tests
let Branch instr x1 x2 resultAddr =
    // Init MachineState
    let addr = 0x80000000L
    let mstate = MachineState.InitMachineState Map.empty RV32i true
    let mstate = mstate.setPC addr
    let mstate = mstate.setRegister 1 x1
    let mstate = mstate.setRegister 2 x2

    let decodedInstr = I.DecodeI instr
    Assert.NotEqual(decodedInstr, I.None)
    let mstate = ExecuteI.ExecuteI decodedInstr mstate
    Assert.Equal(x1, mstate.getRegister 1)
    Assert.Equal(x2, mstate.getRegister 2)
    Assert.Equal(resultAddr, mstate.PC)

[<Theory>]
[<InlineData(0x02208863, 5, 10, 0x80000004L)>]
[<InlineData(0x02208863, 5, -5, 0x80000004L)>]
[<InlineData(0x02208863, 5,  5, 0x80000030L)>]
[<InlineData(0x02208863, -5, -5, 0x80000030L)>]
let ``BEQ: x1 == x2`` (instr, x1, x2, addrRes) =
    Branch instr x1 x2 addrRes
