module Tests

open Microsoft.FSharp.Collections
open Xunit

open ISA.RISCV
open ISA.RISCV.Arch
open ISA.RISCV.Decode

let ALUimmediate instr x2 x3 =
    // Init MachineState
    let addr = 0x80000000L
    let mstate = MachineState.InitMachineState Map.empty RV32i true
    let mstate = mstate.setPC addr
    let mstate = mstate.setRegister 2 x2

    let decodedInstr = I.DecodeI instr
    Assert.NotEqual(decodedInstr, I.None)
    let mstate = ExecuteI.ExecuteI decodedInstr mstate
    Assert.Equal(x2, mstate.getRegister 2)
    Assert.Equal(x3, mstate.getRegister 3)
    Assert.Equal(addr + 4L, mstate.PC)

[<Theory>]
[<InlineData(5, 10)>]
[<InlineData(-5, 0)>]
[<InlineData(-10, -5)>]
let ``ADDI: x3 = x2 + 5`` (x2, x3) =
    ALUimmediate 0x00510193L x2 x3

[<Theory>]
[<InlineData(5, 0)>]
[<InlineData(10, 0)>]
[<InlineData(4, 1)>]
[<InlineData(0, 1)>]
[<InlineData(-4, 1)>]
[<InlineData(-5, 1)>]
[<InlineData(-10, 1)>]
let ``SLTI: x3 = x2 < 5`` (x2, x3) =
    ALUimmediate 0x00512193L x2 x3

[<Theory>]
//[<InlineData(5, 0)>]
//[<InlineData(10, 0)>]
//[<InlineData(4, 1)>]
//[<InlineData(0, 1)>]
[<InlineData(-4, 1)>]
//[<InlineData(-5, 0)>]
//[<InlineData(-10, 0)>]
let ``SLTIU: x3 = unsign x2 < 5`` (x2, x3) =
    ALUimmediate 0x00512193L x2 x3

[<Theory>]
[<InlineData(0b101, 0)>]
[<InlineData(0b010, 0b111)>]
let ``XORI: x3 = x2 ^ 5 (b101)`` (x2, x3) =
    ALUimmediate 0x00514193L x2 x3
