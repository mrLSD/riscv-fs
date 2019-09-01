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
[<InlineData(5, 0)>]
[<InlineData(10, 0)>]
[<InlineData(4, 1)>]
[<InlineData(0, 1)>]
[<InlineData(-4, 0)>]
[<InlineData(-5, 0)>]
[<InlineData(-10, 0)>]
let ``SLTIU: x3 = unsign x2 < unsign 5`` (x2, x3) =
    ALUimmediate 0x00513193L x2 x3

[<Theory>]
[<InlineData(0b101, 0)>]
[<InlineData(0b010, 0b111)>]
[<InlineData(0b011, 0b110)>]
[<InlineData(0b1000, 0b1101)>]
[<InlineData(0b1011, 0b1110)>]
let ``XORI: x3 = x2 ^ 5 (b101)`` (x2, x3) =
    ALUimmediate 0x00514193L x2 x3

//-------------
// TODO: set tests
[<Theory>]
[<InlineData(0b101, 0)>]
let ``ORI: x3 = x2 | 5 (b101)`` (x2, x3) =
    ALUimmediate 0x00516193L x2 x3

[<Theory>]
[<InlineData(0b101, 0)>]
let ``ANDI: x3 = x2 & 5 (b101)`` (x2, x3) =
    ALUimmediate 0x00517193L x2 x3

[<Theory>]
[<InlineData(0b101, 0)>]
let ``SLLI: x3 = x2 >> 5 (b101)`` (x2, x3) =
    ALUimmediate 0x00511193L x2 x3

[<Theory>]
[<InlineData(0b101, 0)>]
let ``SRLI: x3 = x2 << 5 (b101)`` (x2, x3) =
    ALUimmediate 0x00515193L x2 x3

[<Theory>]
[<InlineData(0b101, 0)>]
let ``SRAI: x3 = x2 << 5 (b101)`` (x2, x3) =
    ALUimmediate 0x40515193L x2 x3

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
