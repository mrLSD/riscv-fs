module Tests.rv64i.sys

open Xunit

open ISA.RISCV
open ISA.RISCV.Arch
open ISA.RISCV.MachineState

//===============================================
// System tests
let System instr trap =
    // Init MachineState
    let addr = 0x80000000L
    let mstate = MachineState.InitMachineState Map.empty RV32i true
    let mstate = mstate.setPC addr
    let newmstate = mstate.incPC

    let executor = Decoder.Decode mstate instr
    Assert.NotEqual(executor, None)
    let mstate = executor.Value mstate

    if trap then
        Assert.Equal(addr, mstate.PC)
    else
        Assert.Equal(newmstate.PC, mstate.PC)
    mstate

[<Theory>]
[<InlineData(0x0ff0000f)>]
let ``FENCE`` (instr) =
    let mstate = System instr false
    Assert.Equal(NotRun, mstate.RunState)

[<Theory>]
[<InlineData(0x00000073)>]
let ``ECALL`` (instr) =
    let mstate = System instr true
    Assert.Equal(Trap TrapErrors.ECall, mstate.RunState)


[<Theory>]
[<InlineData(0x00100073)>]
let ``EBREAK`` (instr) =
    let mstate = System instr true
    Assert.Equal(Trap TrapErrors.EBreak, mstate.RunState)
