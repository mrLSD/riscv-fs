module Tests.rv32i.mem

open Xunit

open ISA.RISCV
open ISA.RISCV.Arch
open ISA.RISCV.Utils

//===============================================
// Memory tests

// Load from memory instructions
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

    let executor = Decoder.Decode mstate instr
    Assert.NotEqual(executor, None)
    let mstate = executor.Value mstate
    Assert.Equal(x2, mstate.getRegister 2)
    Assert.Equal(resNumber, mstate.getRegister 3)

// Store to memory instructions
let storeMemory instr x3 x2 imm nBytes =
    // Init MachineState
    let addr = 0x80000000L
    let mstate = MachineState.InitMachineState Map.empty RV32i true
    let mstate = mstate.setPC addr
    let mstate = mstate.setRegister 2 x2
    let mstate = mstate.setRegister 3 x3

    let executor = Decoder.Decode mstate instr
    Assert.NotEqual(executor, None)

    // Get memory value
    let memAddr = x2 + imm
    let mstate = executor.Value mstate
    let memoryRes =
        match nBytes with
        | 1 -> // 1 bytes
            int64(int8((Bits.loadByte mstate.Memory memAddr).Value))
        | 2 -> // 2 bytes
            int64((Bits.loadHalfWord mstate.Memory memAddr).Value)
        | _ -> // 4 bytes
            int64((Bits.loadWord mstate.Memory memAddr).Value)

    Assert.Equal(x2, mstate.getRegister 2)
    Assert.Equal(x3, mstate.getRegister 3)
    Assert.Equal(memoryRes, x3)

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

[<Theory>]
[<InlineData(0x00310523,  100L, 0x1000L,  10L)>]
[<InlineData(0xfe310b23, -100L, 0x1000L, -10L)>]
let ``SB: x3, Imm(x2)`` (instr, x3, x2, imm) =
    storeMemory instr x3 x2 imm 1

[<Theory>]
[<InlineData(0x00311523,  100L, 0x1000L,  10L)>]
[<InlineData(0xfe311b23, -100L, 0x1000L, -10L)>]
let ``SH: x3, Imm(x2)`` (instr, x3, x2, imm) =
    storeMemory instr x3 x2 imm 2

[<Theory>]
[<InlineData(0x00312523,  0x00001ac7L, 0x1000L,  10L)>]
[<InlineData(0xfe312b23,     -100, 0x1000L, -10L)>]
let ``SW: x3, Imm(x2)`` (instr, x3, x2, imm) =
    storeMemory instr x3 x2 imm 4