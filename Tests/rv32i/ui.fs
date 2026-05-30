module Tests.rv32i.ui

open Xunit

open ISA.RISCV
open ISA.RISCV.Arch

//===============================================
// Upper immediate tests
let Ui instr x3 =
    // Init MachineState
    let addr = 0x80000000L
    let mstate = MachineState.InitMachineState Map.empty RV32i true
    let mstate = mstate.setPC addr

    let executor = Decoder.Decode mstate instr
    Assert.NotEqual(executor, None)
    let mstate = executor.Value mstate
    Assert.Equal(x3, mstate.getRegister 3)

[<Theory>]
[<InlineData(0x0000a1b7, 0xA000L)>]
[<InlineData(0x000001b7, 0x0L)>]
[<InlineData(0xfffff1b7, 0xfffffffffffff000L)>]
let ``LUI: x3, imm20`` (instr, x3) =
    Ui instr x3

[<Theory>]
[<InlineData(0x0000a197, 0xffffffff8000a000L)>]
[<InlineData(0x00000197, 0xffffffff80000000L)>]
[<InlineData(0xfffff197, 0x7ffff000L)>]
let ``AUIPC: x3, imm20`` (instr, x3) =
    Ui instr x3
