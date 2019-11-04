module Tests.rv64i.j

open Xunit

open ISA.RISCV
open ISA.RISCV.Arch
open ISA.RISCV.MachineState

//===============================================
// Jump tests
let Jump instr x2 x3 resultAddr =
    // Init MachineState
    let addr = 0x80000000L
    let mstate = MachineState.InitMachineState Map.empty RV32i true
    let mstate = mstate.setPC addr
    let mstate = mstate.setRegister 2 x2
    let resMstate = mstate.incPC

    let executor = Decoder.Decode mstate instr
    Assert.NotEqual(executor, None)
    let mstate = executor.Value mstate

    Assert.Equal(int64 (int32 x2), mstate.getRegister 2)
    let pcs = mstate.setPC (mstate.getRegister 3)
    Assert.Equal(resMstate.PC, pcs.PC)
    Assert.Equal(resultAddr, mstate.PC)

[<Theory>]
[<InlineData(0x018001ef, 0x80000018L)>]
[<InlineData(0xff5ff1ef, 0x7ffffff4L)>]
let ``JAL: x3, addr`` (instr, addrRes) =
    Jump instr 0L 3L addrRes

[<Theory>]
[<InlineData(0x010101e7, 0x80000000L, 0x80000010L)>]
[<InlineData(0xff0101e7, 0x80000000L, 0x7ffffff0L)>]
let ``JALR: x3, x2, addr`` (instr, x2, addrRes) =
    Jump instr x2 3L addrRes