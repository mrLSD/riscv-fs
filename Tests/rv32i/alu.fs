module Tests.rv32i.alu

open Xunit

open ISA.RISCV
open ISA.RISCV.Arch

//===============================================
// ALU tests
let ALU instr x1 x2 x3 =
    // Init MachineState
    let addr = 0x80000000L
    let mstate = MachineState.InitMachineState Map.empty RV32i true
    let mstate = mstate.setPC addr
    let mstate = mstate.setRegister 1 x1
    let mstate = mstate.setRegister 2 x2

    let executor = Decoder.Decode instr
    Assert.NotEqual(executor, None)
    let mstate = executor.Value mstate
    Assert.Equal(x1, mstate.getRegister 1)
    Assert.Equal(x2, mstate.getRegister 2)
    Assert.Equal(x3, mstate.getRegister 3)
    Assert.Equal(addr + 4L, mstate.PC)

[<Theory>]
[<InlineData(10, 20, 30)>]
[<InlineData(0, 20, 20)>]
[<InlineData(-10, 20, 10)>]
[<InlineData(-40, 20, -20)>]
[<InlineData(0xFFFFFFFF, 10, 9)>] // Overflow
let ``ADD: x3 = x2 + x1`` (x1, x2, x3) =
    ALU 0x001101b3L x1 x2 x3

[<Theory>]
[<InlineData(20, 10, -10)>]
[<InlineData(10, 20, 10)>]
[<InlineData(0, 20, 20)>]
[<InlineData(10, 0, -10)>]
[<InlineData(-10, -20, -10)>]
[<InlineData(10, -20, -30)>]
[<InlineData(10, 0xFFFFFFFF, -11)>]
[<InlineData(0xFFFFFFFF, 10, 11)>] // Overflow
let ``SUB: x3 = x2 - x1`` (x1, x2, x3) =
    ALU 0x401101b3L x1 x2 x3

[<Theory>]
[<InlineData(5, 0b101101, 0b10110100000)>]
[<InlineData(0, 0b101101, 0b101101)>]
let ``SLL: x3 = x2 << x1`` (x1, x2, x3) =
    ALU 0x001111b3L x1 x2 x3

[<Theory>]
[<InlineData(10, 20, 0)>]
[<InlineData(20, 20, 0)>]
[<InlineData(20, 10, 1)>]
[<InlineData(20, -10, 1)>]
[<InlineData(-5, -10, 1)>]
[<InlineData(-10, 10, 0)>]
let ``SLT: x3 = x2 < x1`` (x1, x2, x3) =
    ALU 0x001121b3L x1 x2 x3

[<Theory>]
[<InlineData(10, 20, 0)>]
[<InlineData(20, 20, 0)>]
[<InlineData(20, 10, 1)>]
[<InlineData(20, -10, 0)>]
[<InlineData(-5, -10, 1)>]
[<InlineData(-10, 10, 1)>]
let ``SLTU: x3 = unsign x2 < unsign x1`` (x1, x2, x3) =
    ALU 0x001131b3L x1 x2 x3

[<Theory>]
[<InlineData(0b101, 0b101, 0)>]
[<InlineData(0b101, 0b010, 0b111)>]
[<InlineData(0b101, 0b011, 0b110)>]
[<InlineData(0b101, 0b1000, 0b1101)>]
[<InlineData(0b101, 0b1011, 0b1110)>]
let ``XOR: x3 = x2 ^ x1`` (x1, x2, x3) =
    ALU 0x001141b3L x1 x2 x3

[<Theory>]
[<InlineData(0b101, 0b1011001101, 0b0000010110)>]
[<InlineData(0b101, 0b11001100101, 0b00000110011)>]
[<InlineData(0b101, 0b11110000111100000000000000001111, 0b00000111100001111000000000000000)>]
let ``SRL: x3 = x2 >> x1`` (x1, x2, x3) =
    ALU 0x001151b3L x1 x2 x3

[<Theory>]
[<InlineData(0b101, 0b1011001101, 0b0000010110)>]
[<InlineData(0b101, 0b11110000111100000000000000001111, 0b11111111100001111000000000000000)>]
let ``SRA: x3 = x2 >> x1`` (x1, x2, x3) =
    ALU 0x401151b3L x1 x2 x3

[<Theory>]
[<InlineData(0b101, 0b101, 0b101)>]
[<InlineData(0b101, 0b110, 0b111)>]
[<InlineData(0b101, 0b011, 0b111)>]
[<InlineData(0b101, 0b1111, 0b1111)>]
[<InlineData(0b101, 0b1101, 0b1101)>]
let ``OR: x3 = x2 | x1`` (x1, x2, x3) =
    ALU 0x001161b3L x1 x2 x3

[<Theory>]
[<InlineData(0b101, 0b101, 0b101)>]
[<InlineData(0b101, 0b111, 0b101)>]
[<InlineData(0b101, 0b110, 0b100)>]
[<InlineData(0b101, 0b1011, 0b0001)>]
let ``AND: x3 = x2 & x1`` (x1, x2, x3) =
    ALU 0x001171b3L x1 x2 x3
