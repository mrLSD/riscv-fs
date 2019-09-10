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
[<InlineData(0xfe208ee3, 5, 10, 0x80000004L)>]
[<InlineData(0xfe208ee3, 5, -5, 0x80000004L)>]
[<InlineData(0xfe208ee3, 5,  5, 0x7ffffffcL)>]
[<InlineData(0xfe208ee3, -5, -5, 0x7ffffffcL)>]
let ``BEQ: x1 == x2`` (instr, x1, x2, addrRes) =
    Branch instr x1 x2 addrRes

[<Theory>]
[<InlineData(0x02209463, 5, 10, 0x80000028L)>]
[<InlineData(0x02209463, 5, -5, 0x80000028L)>]
[<InlineData(0x02209463, 5,  5, 0x80000004L)>]
[<InlineData(0x02209463, -5, -5, 0x80000004L)>]
[<InlineData(0xfe209ae3, 5, 10, 0x7ffffff4L)>]
[<InlineData(0xfe209ae3, 5, -5, 0x7ffffff4L)>]
[<InlineData(0xfe209ae3, 5,  5, 0x80000004L)>]
[<InlineData(0xfe209ae3, -5, -5, 0x80000004L)>]
let ``BNE: x1 <> x2`` (instr, x1, x2, addrRes) =
    Branch instr x1 x2 addrRes
