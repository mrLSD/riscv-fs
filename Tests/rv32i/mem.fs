module Tests.rv32i.mem

open Xunit

open ISA.RISCV
open ISA.RISCV.Arch
open ISA.RISCV.Decode

//===============================================
// Upper immediate tests
let loadMemory instr x2 imm nBytes unsign =
    // Init MachineState
    let addr = 0x80000000L
    let mstate = MachineState.InitMachineState Map.empty RV32i true
    let mstate = mstate.setPC addr
    let mstate = mstate.setRegister 2 x2

    // Set memory value
    let memAddr = x2 + imm
    let (mstate, resNumber) =
        match nBytes with
        | 1 -> // 1 bytes
            let data = if unsign then int64(0x85uy) else int64(0x85y)
            (mstate.setMemoryByte memAddr 0x85uy, data)
        | 2 -> // 2 bytes
            let data = if unsign then int64(0xa10fus) else int64(0xa10fs)
            let mstate = mstate.setMemoryByte memAddr 0x0fuy
            (mstate.setMemoryByte (memAddr+1L) 0xa1uy, data)
        | _ -> // 4 bytes
            let mstate = mstate.setMemoryByte memAddr 0x0fuy
            let mstate = mstate.setMemoryByte (memAddr+1L) 0xa1uy
            let mstate = mstate.setMemoryByte (memAddr+2L) 0xb2uy
            (mstate.setMemoryByte (memAddr+3L) 0xc3uy, int64(0xc3b2a10fl))

    let decodedInstr = I.DecodeI instr
    Assert.NotEqual(decodedInstr, I.None)
    let mstate = ExecuteI.ExecuteI decodedInstr mstate
    Assert.Equal(x2, mstate.getRegister 2)
    Assert.Equal(resNumber, mstate.getRegister 3)

[<Theory>]
[<InlineData(0x00a10183, 0x1000L,  10L)>]
[<InlineData(0xff610183, 0x1000L, -10L)>]
let ``LB: x3, Imm(x2)`` (instr, x2, imm) =
    loadMemory instr x2 imm 1 false

[<Theory>]
[<InlineData(0x00a11183, 0x1000L,  10L)>]
[<InlineData(0xff611183, 0x1000L, -10L)>]
let ``LH: x3, Imm(x2)`` (instr, x2, imm) =
    loadMemory instr x2 imm 2 false

[<Theory>]
[<InlineData(0x00a12183, 0x1000L,  10L)>]
[<InlineData(0xff612183, 0x1000L, -10L)>]
let ``LW: x3, Imm(x2)`` (instr, x2, imm) =
    loadMemory instr x2 imm 4 false

[<Theory>]
[<InlineData(0x00a14183, 0x1000L,  10L)>]
[<InlineData(0xff614183, 0x1000L, -10L)>]
let ``LBU: x3, Imm(x2)`` (instr, x2, imm) =
    loadMemory instr x2 imm 1 true

[<Theory>]
[<InlineData(0x00a15183, 0x1000L,  10L)>]
[<InlineData(0xff615183, 0x1000L, -10L)>]
let ``LHU: x3, Imm(x2)`` (instr, x2, imm) =
    loadMemory instr x2 imm 2 true
