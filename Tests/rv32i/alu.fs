module Tests.rv32i.alu

open Xunit

open ISA.RISCV
open ISA.RISCV.Arch
open ISA.RISCV.Decode

//===============================================
// ALU tests
let ALU instr x1 x2 x3 =
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
    Assert.Equal(x3, mstate.getRegister 3)
    Assert.Equal(addr + 4L, mstate.PC)
(*
[<Theory>]
[<InlineData(0b101, 0)>]
let ``ADD: x3 = x2 + x1`` (x1, x2, x3) =
    ALU 0x001101b3L x1 x2 x3

[<Theory>]
[<InlineData(0b101, 0)>]
let ``SUB: x3 = x2 - x1`` (x1, x2, x3) =
    ALU 0x401101b3L x1 x2 x3

[<Theory>]
[<InlineData(0b101, 0)>]
let ``SLL: x3 = x2 << x1`` (x1, x2, x3) =
    ALU 0x001111b3L x1 x2 x3

[<Theory>]
[<InlineData(0b101, 0)>]
let ``SLT: x3 = x2 < x1`` (x1, x2, x3) =
    ALU 0x001121b3L x1 x2 x3

[<Theory>]
[<InlineData(0b101, 0)>]
let ``SLTU: x3 = unsign x2 << unsign x1`` (x1, x2, x3) =
    ALU 0x001131b3L x1 x2 x3

[<Theory>]
[<InlineData(0b101, 0)>]
let ``XOR: x3 = x2 ^ x1`` (x1, x2, x3) =
    ALU 0x001141b3L x1 x2 x3

[<Theory>]
[<InlineData(0b101, 0)>]
let ``SRL: x3 = x2 >> x1`` (x1, x2, x3) =
    ALU 0x001151b3L x1 x2 x3

[<Theory>]
[<InlineData(0b101, 0)>]
let ``SRA: x3 = x2 >> x1`` (x1, x2, x3) =
    ALU 0x401151b3L x1 x2 x3

[<Theory>]
[<InlineData(0b101, 0)>]
let ``OR: x3 = x2 | x1`` (x1, x2, x3) =
    ALU 0x001161b3L x1 x2 x3

[<Theory>]
[<InlineData(0b101, 0)>]
let ``AND: x3 = x2 & x1`` (x1, x2, x3) =
    ALU 0x001171b3L x1 x2 x3
*)