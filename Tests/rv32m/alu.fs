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
[<InlineData(10, 20, 200)>]
[<InlineData(0, 20, 0)>]
[<InlineData(-10, 20, -200)>]
[<InlineData(10, -20, -200)>]
[<InlineData(-10, -20, 200)>]
[<InlineData(0xFFFFFFFF, 10, -10)>]
let ``MUL: x3 = x2 * x1`` (x1, x2, x3) =
    ALU 0x021101b3 x1 x2 x3

let ``MULH: x3 = half (x2 * x1)`` (x1, x2, x3) =
    ALU 0x021111b3 x1 x2 x3

let ``MULHSU: x3 = half (sign x2 * unsign x1)`` (x1, x2, x3) =
    ALU 0x021121b3 x1 x2 x3

let ``MULHU: x3 = half (unsign x2 * unsign x1)`` (x1, x2, x3) =
    ALU 0x021131b3 x1 x2 x3

let ``DIV: x3 = x2 / x1`` (x1, x2, x3) =
    ALU 0x021141b3 x1 x2 x3

let ``DIVU: x3 = (unsign x2) / (unsign x1)`` (x1, x2, x3) =
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