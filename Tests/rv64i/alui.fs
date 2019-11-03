module Tests.rv64i.alui

open Xunit

open ISA.RISCV
open ISA.RISCV.Arch
open ISA.RISCV.Decode

//===============================================
// ALU Immediate Word tests
let ALUimmediateWord instr x2 x3 =
    // Init MachineState
    let addr = 0x80000000L
    let mstate = MachineState.InitMachineState Map.empty RV32i true
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
let ``ADDIW: x3 = x2 + 5`` (x2, x3) =
    ALUimmediateWord 0x00510193 x2 x3

[<Theory>]
[<InlineData(0b11001101, 0b1100110100000)>]
[<InlineData(0b11001110, 0b1100111000000)>]
let ``SLLI: x3 = x2 << 5 (b101)`` (x2, x3) =
    ALUimmediateWord 0x00511193 x2 x3

[<Theory>]
[<InlineData(0b1011001101, 0b0000010110)>]
[<InlineData(0b11001100101, 0b00000110011)>]
[<InlineData(0b11110000111100000000000000001111, 0b00000111100001111000000000000000)>]
let ``SRLI: x3 = x2 >> 5 (b101)`` (x2, x3) =
    ALUimmediateWord 0x00515193 x2 x3

[<Theory>]
[<InlineData(0b1011001101, 0b0000010110)>]
[<InlineData(0b11110000111100000000000000001111, 0b11111111100001111000000000000000)>]
let ``SRAI: x3 = x2 >> 5 (b101)`` (x2, x3) =
    ALUimmediateWord 0x40515193 x2 x3
