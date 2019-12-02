module Tests.rv64m.alu

open Xunit

open ISA.RISCV
open ISA.RISCV.Arch

//===============================================
// ALU `x64` tests
let ALU instr x1 x2 x3 =
    // Init MachineState
    let addr = 0x80000000L
    let mstate = MachineState.InitMachineState Map.empty RV64im true
    let mstate = mstate.setPC addr
    let mstate = mstate.setRegister 1 x1
    let mstate = mstate.setRegister 2 x2

    let executor = Decoder.Decode mstate instr
    Assert.NotEqual(executor, None)
    let mstate = executor.Value mstate
    Assert.Equal(x1, mstate.getRegister 1)
    Assert.Equal(x2, mstate.getRegister 2)
    Assert.Equal(x3, mstate.getRegister 3)
    Assert.Equal(addr + 4L, mstate.PC)

[<Theory>]
[<InlineData(200, 10, 20)>]
[<InlineData(0x0000000000001200, 0x0000000000007e00, 0x6db6db6db6db6db7L)>]
[<InlineData(0x0000000000001240, 0x0000000000007fc0, 0x6db6db6db6db6db7L)>]
[<InlineData(0x00000000, 0x00000000, 0x00000000)>]
[<InlineData(0x00000001, 0x00000001, 0x00000001)>]
[<InlineData(0x00000015, 0x00000003, 0x00000007)>]
[<InlineData(0x0000000000000000, 0x0000000000000000, 0xffffffffffff8000L)>]
[<InlineData(0x0000000000000000, 0xffffffff80000000L, 0x00000000)>]
[<InlineData(0x0000400000000000L, 0xffffffff80000000L, 0xffffffffffff8000L)>]
[<InlineData(0x000000000000ff7f, 0xaaaaaaaaaaaaaaabL, 0x000000000002fe7d)>]
[<InlineData(0x000000000000ff7f, 0x000000000002fe7d, 0xaaaaaaaaaaaaaaabL)>]
let ``MUL: x3 = x2 * x1`` ( x3, x1, x2) =
    ALU 0x021101b3 x1 x2 x3

[<Theory>]
[<InlineData(0x00000000, 0x00000000, 0x00000000)>]
[<InlineData(0x00000000, 0x00000001, 0x00000001)>]
[<InlineData(0x00000000, 0x00000003, 0x00000007)>]
[<InlineData(0x0000000000000000, 0x0000000000000000, 0xffffffffffff8000L)>]
[<InlineData(0x0000000000000000, 0xffffffff80000000L, 0x00000000)>]
[<InlineData(0x0000000000000000, 0xffffffff80000000L, 0xffffffffffff8000L)>]
let ``MULH: x3 = half (x2 * x1)`` (x3, x1, x2) =
    ALU 0x021111b3 x1 x2 x3

[<Theory>]
[<InlineData(0x00000000, 0x00000000, 0x00000000)>]
[<InlineData(0x00000000, 0x00000001, 0x00000001)>]
[<InlineData(0x00000000, 0x00000003, 0x00000007)>]
[<InlineData(0x0000000000000000, 0x0000000000000000, 0xffffffffffff8000L)>]
[<InlineData(0x0000000000000000, 0xffffffff80000000L, 0x00000000)>]
//[<InlineData(0xffffffff7fff8000L, 0xffffffff80000000L, 0xffffffffffff8000L)>]
//[<InlineData(0x000000000001fefe, 0xaaaaaaaaaaaaaaabL, 0x000000000002fe7d)>]
//[<InlineData(0x000000000001fefe, 0x000000000002fe7d, 0xaaaaaaaaaaaaaaabL)>]
let ``MULHU: x3 = half (unsign x2 * unsign x1)`` (x3, x1, x2) =
    ALU 0x021131b3 x1 x2 x3

[<Theory>]
[<InlineData(0x00000000, 0x00000000, 0x00000000)>]
[<InlineData(0x00000000, 0x00000001, 0x00000001)>]
[<InlineData(0x00000000, 0x00000003, 0x00000007)>]
[<InlineData(0x0000000000000000, 0x0000000000000000, 0xffffffffffff8000L)>]
[<InlineData(0x0000000000000000, 0xffffffff80000000L, 0x00000000)>]
//[<InlineData(0xffffffff80000000L, 0xffffffff80000000L, 0xffffffffffff8000L)>]
let ``MULHSU: x3 = half (sign x2 * unsign x1)`` (x3, x2, x1) =
    ALU 0x021121b3 x1 x2 x3

[<Theory>]
[<InlineData(3, 20, 6)>]
[<InlineData(-3, -20, 6)>]
[<InlineData(-3, 20, -6)>]
[<InlineData(3, -20, -6)>]
[<InlineData(0x8000000000000000L, 0x8000000000000000L, 1)>]
[<InlineData(0x8000000000000000L, 0x8000000000000000L, -1)>]
[<InlineData(-1, 0x8000000000000000L, 0)>]
[<InlineData(-1, 1, 0)>]
[<InlineData(-1, 0, 0)>]
let ``DIV: x3 = x2 / x1`` (x3, x2, x1) =
    ALU 0x021141b3 x1 x2 x3

[<Theory>]
[<InlineData(3, 20, 6)>]
[<InlineData(3074457345618258599L, -20, 6)>]
[<InlineData(0, 20, -6)>]
[<InlineData(0, -20, -6)>]
[<InlineData(0x8000000000000000L, 0x8000000000000000L, 1)>]
[<InlineData(0, 0x8000000000000000L, -1)>]
[<InlineData(-1, 0x8000000000000000L, 0)>]
[<InlineData(-1, 1, 0)>]
[<InlineData(-1, 0, 0)>]
let ``DIVU: x3 = (unsign x2) / (unsign x1)`` (x3, x2, x1) =
    ALU 0x021151b3 x1 x2 x3

[<Theory>]
[<InlineData( 2, 20,  6)>]
[<InlineData(-2,-20,  6)>]
[<InlineData( 2, 20, -6)>]
[<InlineData(-2,-20, -6)>]
[<InlineData( 0, 0x8000000000000000L,  1)>]
[<InlineData( 0, 0x8000000000000000L, -1)>]
[<InlineData(0x8000000000000000L, 0x8000000000000000L, 0)>]
[<InlineData(1, 1, 0)>]
[<InlineData(0, 0, 0)>]
let ``REM: x3 = x2 % x1`` (x3, x2, x1) =
    ALU 0x021161b3 x1 x2 x3

[<Theory>]
[<InlineData( 2, 20, 6)>]
[<InlineData( 2,-20, 6)>]
[<InlineData( 20, 20, -6)>]
[<InlineData(-20,-20, -6)>]
[<InlineData( 0, 0x8000000000000000L,  1)>]
[<InlineData(0x8000000000000000L, 0x8000000000000000L, -1)>]
[<InlineData(0x8000000000000000L, 0x8000000000000000L, 0)>]
[<InlineData(1, 1, 0)>]
[<InlineData(0, 0, 0)>]
let ``REMU: x3 = (unsign x2) % (unsign x1)`` (x3, x2, x1) =
    ALU 0x021171b3 x1 x2 x3
