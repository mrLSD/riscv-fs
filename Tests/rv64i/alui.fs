module Tests.rv64i.alui

open Xunit

open ISA.RISCV
open ISA.RISCV.Arch

//===============================================
// ALU Immediate Word tests
let ALUimmediate instr x2 x3 =
    // Init MachineState
    let addr = 0x80000000L
    let mstate = MachineState.InitMachineState Map.empty RV64i true
    let mstate = mstate.setPC addr
    let mstate = mstate.setRegister 2 x2

    let executor = Decoder.Decode mstate instr
    Assert.NotEqual(executor, None)
    let mstate = executor.Value mstate
    Assert.Equal(x2, mstate.getRegister 2)
    Assert.Equal(x3, mstate.getRegister 3)
    Assert.Equal(addr + 4L, mstate.PC)

[<Theory>]
[<InlineData(5, 10)>]
[<InlineData(-5, 0)>]
[<InlineData(-10, -5)>]
let ``ADDI: x3 = x2 + 5`` (x2, x3) =
    ALUimmediate 0x00510193 x2 x3

[<Theory>]
[<InlineData(5, 0)>]
[<InlineData(10, 0)>]
[<InlineData(4, 1)>]
[<InlineData(0, 1)>]
[<InlineData(-4, 1)>]
[<InlineData(-5, 1)>]
[<InlineData(-10, 1)>]
let ``SLTI: x3 = x2 < 5`` (x2, x3) =
    ALUimmediate 0x00512193 x2 x3

[<Theory>]
[<InlineData(5, 0)>]
[<InlineData(10, 0)>]
[<InlineData(4, 1)>]
[<InlineData(0, 1)>]
[<InlineData(-4, 0)>]
[<InlineData(-5, 0)>]
[<InlineData(-10, 0)>]
let ``SLTIU: x3 = unsign x2 < unsign 5`` (x2, x3) =
    ALUimmediate 0x00513193 x2 x3

[<Theory>]
[<InlineData(0b101, 0)>]
[<InlineData(0b010, 0b111)>]
[<InlineData(0b011, 0b110)>]
[<InlineData(0b1000, 0b1101)>]
[<InlineData(0b1011, 0b1110)>]
let ``XORI: x3 = x2 ^ 5 (b101)`` (x2, x3) =
    ALUimmediate 0x00514193 x2 x3

[<Theory>]
[<InlineData(0b101, 0b101)>]
[<InlineData(0b110, 0b111)>]
[<InlineData(0b011, 0b111)>]
[<InlineData(0b1111, 0b1111)>]
[<InlineData(0b1101, 0b1101)>]
let ``ORI: x3 = x2 | 5 (b101)`` (x2, x3) =
    ALUimmediate 0x00516193 x2 x3

[<Theory>]
[<InlineData(0b101, 0b101)>]
[<InlineData(0b111, 0b101)>]
[<InlineData(0b110, 0b100)>]
[<InlineData(0b1011, 0b0001)>]
let ``ANDI: x3 = x2 & 5 (b101)`` (x2, x3) =
    ALUimmediate 0x00517193 x2 x3

[<Theory>]
[<InlineData(0b11001101, 0b1100110100000)>]
[<InlineData(0b11001110, 0b1100111000000)>]
[<InlineData(0x8FAFCF1F8AF300C7L, 0xf5f9e3f15e6018e0L)>]
[<InlineData(0x00AFCF1F8AF300C7L, 0x15f9e3f15e6018e0L)>]
let ``SLLI: x3 = x2 << 5 (b101)`` (x2, x3) =
    ALUimmediate 0x00511193 x2 x3

[<Theory>]
[<InlineData(0b1011001101, 0b0000010110)>]
[<InlineData(0b11001100101, 0b00000110011)>]
[<InlineData(0b11110000111100000000000000001111, 0x7ffffffff878000L)>]
[<InlineData(0x8FAFCF1F8AF300C7L, 0x47d7e78fc579806L)>]
[<InlineData(0xFFDFCF1F8AF300C7L, 0x7fefe78fc579806L)>]
let ``SRLI: x3 = x2 >> 5 (b101)`` (x2 : int64, x3 : int64) =
    ALUimmediate 0x00515193 x2 x3

[<Theory>]
[<InlineData(0b1011001101, 0b0000010110)>]
[<InlineData(0b11110000111100000000000000001111, 0b11111111100001111000000000000000)>]
[<InlineData(0x0FAFCF1F8AF300C7L, 0x7d7e78fc579806L)>]
[<InlineData(0x8FAFCF1F8AF300C7L, 0xfc7d7e78fc579806L)>]
let ``SRAI: x3 = x2 >> 5 (b101)`` (x2, x3) =
    ALUimmediate 0x40515193 x2 x3

[<Theory>]
[<InlineData(5, 10)>]
[<InlineData(-5, 0)>]
[<InlineData(0x00000FFAFFFFFFF0L, 0xFFFFFFFFFFFFFFF5L)>]
let ``ADDIW: x3 = x2 + 5`` (x2 : int64, x3 : int64) =
    ALUimmediate 0x0051019b x2 x3

[<Theory>]
[<InlineData(0b11001101, 0b1100110100000)>]
[<InlineData(0b11001110, 0b1100111000000)>]
[<InlineData(0xFFFFFFFF8AF300C7L, 0x5e6018e0L)>]
[<InlineData(0xFFFFFFFF8FF3F0C7L, 0xfffffffffe7e18e0L)>]
let ``SLLIW: x3 = x2 << 5 (b101)`` (x2, x3) =
    ALUimmediate 0x0051119b x2 x3

[<Theory>]
[<InlineData(0b1011001101, 0b0000010110)>]
[<InlineData(0b11001100101, 0b00000110011)>]
[<InlineData(0b11110000111100000000000000001111, 0x7878000L)>]
[<InlineData(0xFFFFFFFF8AF300C7L, 0x4579806L)>]
let ``SRLIW: x3 = x2 >> 5 (b101)`` (x2 : int64, x3 : int64) =
    ALUimmediate 0x0051519b x2 x3

[<Theory>]
[<InlineData(0b1011001101, 0b0000010110)>]
[<InlineData(0xFFFFFFFF6AF300C7L, 0x3579806L)>]
[<InlineData(0xFFFFFFFF8AF300C7L, 0xfffffffffc579806L)>]
let ``SRAIW: x3 = x2 >> 5 (b101)`` (x2, x3) =
    ALUimmediate 0x4051519b x2 x3
