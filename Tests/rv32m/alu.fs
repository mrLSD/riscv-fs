module Tests.rvM.alu

open Xunit

open ISA.RISCV
open ISA.RISCV.Arch

//===============================================
// ALU tests
let ALU instr x1 x2 x3 =
    // Init MachineState
    let addr = 0x80000000L
    let mstate = MachineState.InitMachineState Map.empty RV32im true
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
[<InlineData(0, 20, 0)>]
[<InlineData(-200, -10, 20)>]
[<InlineData(-200, 10, -20)>]
[<InlineData( 200,-10, -20)>]
[<InlineData(-10, 0xFFFFFFFF, 10)>]
[<InlineData(0, -10, 0)>]
[<InlineData(0x80000000, 0x80000000, -1)>]
[<InlineData(0x80000000, 0x80000000, 1)>]
[<InlineData(0, 0x80000000, -10)>]
[<InlineData(0, 0x80000000, 10)>]
[<InlineData(-10, 0x7FFFFFFF, 10)>]
[<InlineData(-10, 10, 0x7FFFFFFF)>]
[<InlineData(0x80000000, 0x7FFFFFFF, 0x80000000)>]
[<InlineData(1, 0x7FFFFFFF, 0x7FFFFFFF)>]
[<InlineData(0, 0x80000000, 0x80000000)>]
[<InlineData(0x00001200, 0x00007e00, 0xb6db6db7)>]
[<InlineData(0x00001240, 0x00007fc0, 0xb6db6db7)>]
[<InlineData(0x00000000, 0x00000000, 0x00000000)>]
[<InlineData(0x00000001, 0x00000001, 0x00000001)>]
[<InlineData(0x00000015, 0x00000003, 0x00000007)>]
[<InlineData(0x00000000, 0x00000000, 0xffff8000)>]
[<InlineData(0x00000000, 0x80000000, 0x00000000)>]
[<InlineData(0x00000000, 0x80000000, 0xffff8000)>]
[<InlineData(0x0000ff7f, 0xaaaaaaab, 0x0002fe7d)>]
[<InlineData(0x0000ff7f, 0x0002fe7d, 0xaaaaaaab)>]
[<InlineData(0x00000000, 0xff000000, 0xff000000)>]
[<InlineData(0x00000001, 0xffffffff, 0xffffffff)>]
[<InlineData(0xffffffff, 0xffffffff, 0x00000001)>]
[<InlineData(0xffffffff, 0x00000001, 0xffffffff)>]
let ``MUL: x3 = x2 * x1`` ( x3, x1, x2) =
    ALU 0x021101b3 x1 x2 x3

[<Theory>]
[<InlineData(0x00000000, 0x00000000, 0x00000000)>]
[<InlineData(0x00000000, 0x00000001, 0x00000001)>]
[<InlineData(0x00000000, 0x00000003, 0x00000007)>]
[<InlineData(0x00000000, 0x00000000, 0xffff8000)>]
[<InlineData(0x00000000, 0x80000000, 0x00000000)>]
[<InlineData(0xffff0081, 0xaaaaaaab, 0x0002fe7d)>]
[<InlineData(0xffff0081, 0x0002fe7d, 0xaaaaaaab)>]
[<InlineData(0x00010000, 0xff000000, 0xff000000)>]
[<InlineData(0x00000000, 0xffffffff, 0xffffffff)>]
[<InlineData(0xffffffff, 0xffffffff, 0x00000001)>]
[<InlineData(0xffffffff, 0x00000001, 0xffffffff)>]
let ``MULH: x3 = half (x2 * x1)`` (x3, x1, x2) =
    ALU 0x021111b3 x1 x2 x3

[<Theory>]
[<InlineData(0x00000000, 0x00000000, 0x00000000)>]
[<InlineData(0x00000000, 0x00000001, 0x00000001)>]
[<InlineData(0x00000000, 0x00000003, 0x00000007)>]
[<InlineData(0x00000000, 0x00000000, 0xffff8000)>]
[<InlineData(0x00000000, 0x80000000, 0x00000000)>]
[<InlineData(0x80004000, 0x80000000, 0xffff8000)>]
[<InlineData(0xffff0081, 0xaaaaaaab, 0x0002fe7d)>]
[<InlineData(0x0001fefe, 0x0002fe7d, 0xaaaaaaab)>]
[<InlineData(0xff010000, 0xff000000, 0xff000000)>]
[<InlineData(0xffffffff, 0xffffffff, 0xffffffff)>]
[<InlineData(0xffffffff, 0xffffffff, 0x00000001)>]
[<InlineData(0x00000000, 0x00000001, 0xffffffff)>]
let ``MULHSU: x3 = half (sign x2 * unsign x1)`` (x3, x2, x1) =
    ALU 0x021121b3 x1 x2 x3

[<Theory>]
[<InlineData(0x00000000, 0x00000000, 0x00000000)>]
[<InlineData(0x00000000, 0x00000001, 0x00000001)>]
[<InlineData(0x00000000, 0x00000003, 0x00000007)>]
[<InlineData(0x00000000, 0x00000000, 0xffff8000)>]
[<InlineData(0x00000000, 0x80000000, 0x00000000)>]
[<InlineData(0x7fffc000, 0x80000000, 0xffff8000)>]
[<InlineData(0x0001fefe, 0xaaaaaaab, 0x0002fe7d)>]
[<InlineData(0x0001fefe, 0x0002fe7d, 0xaaaaaaab)>]
[<InlineData(0xfe010000, 0xff000000, 0xff000000)>]
[<InlineData(0xfffffffe, 0xffffffff, 0xffffffff)>]
[<InlineData(0x00000000, 0xffffffff, 0x00000001)>]
[<InlineData(0x00000000, 0x00000001, 0xffffffff)>]
let ``MULHU: x3 = half (unsign x2 * unsign x1)`` (x3, x1, x2) =
    ALU 0x021131b3 x1 x2 x3

[<Theory>]
[<InlineData(2, 4, 2)>]
[<InlineData(2, 5, 2)>]
[<InlineData(3, 6, 2)>]
[<InlineData(3, 15, 4)>]
[<InlineData(0, -1, 3)>]
[<InlineData(0, 1, -3)>]
[<InlineData(-2, 4, -2)>]
[<InlineData(-2, -4, 2)>]
[<InlineData(2, -4, -2)>]
[<InlineData(0, 0, 2)>]
[<InlineData(-1, 2, 0)>]
[<InlineData(0x80000000, 0x80000000, -1)>]
[<InlineData(0x80000000, 0x80000000, 1)>]
[<InlineData(-1, 0x80000000, 0)>]
[<InlineData(-1, 1, 0)>]
[<InlineData(-1, 0, 0)>]
let ``DIV: x3 = x2 / x1`` (x3, x2, x1) =
    ALU 0x021141b3 x1 x2 x3

[<Theory>]
[<InlineData(2, 4, 2)>]
[<InlineData(2, 5, 2)>]
[<InlineData(3, 6, 2)>]
[<InlineData(3, 15, 4)>]
[<InlineData(0, 1, 3)>]
[<InlineData(0x55555555, -1, 3)>]
[<InlineData(0, 1, -3)>]
[<InlineData(0, 4, -2)>]
[<InlineData(715827879, -20, 6)>]
[<InlineData(0, -4, -2)>]
[<InlineData(0, 0, 2)>]
[<InlineData(0xFFFFFFFF, 2, 0)>]
[<InlineData(0x7FFFFFFB, -10, 2)>]
[<InlineData(0x80000000, 0x80000000, 1)>]
[<InlineData(0, 0x80000000, -1)>]
[<InlineData(-1, 0x80000000, 0)>]
let ``DIVU: x3 = (unsign x2) / (unsign x1)`` (x3, x2, x1) =
    ALU 0x021151b3 x1 x2 x3

let ``REM: x3 = x2 % x1`` (x1, x2, x3) =
    ALU 0x021161b3 x1 x2 x3

let ``DIVU: x3 = (unsign x2) % (unsign x1)`` (x1, x2, x3) =
    ALU 0x021171b3 x1 x2 x3
// 021101bb
// 021141bb
// 021151bb
// 021161bb
// 021171bb