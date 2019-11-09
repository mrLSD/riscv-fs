module Tests.rv64i.br

open Xunit

open ISA.RISCV
open ISA.RISCV.Arch

//===============================================
// Branch tests
let Branch instr x1 x2 resultAddr =
    // Init MachineState
    let addr = 0x80000000L
    let mstate = MachineState.InitMachineState Map.empty RV64i true
    let mstate = mstate.setPC addr
    let mstate = mstate.setRegister 1 x1
    let mstate = mstate.setRegister 2 x2

    let executor = Decoder.Decode mstate instr
    Assert.NotEqual(executor, None)
    let mstate = executor.Value mstate
    Assert.Equal(x1, mstate.getRegister 1)
    Assert.Equal(x2, mstate.getRegister 2)
    Assert.Equal(resultAddr, mstate.PC)

[<Theory>]
[<InlineData(0x02208863,  5, 10, 0x80000004L)>]
[<InlineData(0x02208863,  5, -5, 0x80000004L)>]
[<InlineData(0x02208863,  5,  5, 0x80000030L)>]
[<InlineData(0x02208863, -5, -5, 0x80000030L)>]
[<InlineData(0xfe208ee3,  5, 10, 0x80000004L)>]
[<InlineData(0xfe208ee3,  5, -5, 0x80000004L)>]
[<InlineData(0xfe208ee3,  5,  5, 0x7ffffffcL)>]
[<InlineData(0xfe208ee3, -5, -5, 0x7ffffffcL)>]
let ``BEQ: x1 == x2`` (instr, x1, x2, addrRes) =
    Branch instr x1 x2 addrRes

[<Theory>]
[<InlineData(0x02209463,  5, 10, 0x80000028L)>]
[<InlineData(0x02209463,  5, -5, 0x80000028L)>]
[<InlineData(0x02209463,  5,  5, 0x80000004L)>]
[<InlineData(0x02209463, -5, -5, 0x80000004L)>]
[<InlineData(0xfe209ae3,  5, 10, 0x7ffffff4L)>]
[<InlineData(0xfe209ae3,  5, -5, 0x7ffffff4L)>]
[<InlineData(0xfe209ae3,  5,  5, 0x80000004L)>]
[<InlineData(0xfe209ae3, -5, -5, 0x80000004L)>]
let ``BNE: x1 <> x2`` (instr, x1, x2, addrRes) =
    Branch instr x1 x2 addrRes

[<Theory>]
[<InlineData(0x0220c063,  5,  10, 0x80000020L)>]
[<InlineData(0x0220c063, -5,  10, 0x80000020L)>]
[<InlineData(0x0220c063,  5,  -5, 0x80000004L)>]
[<InlineData(0x0220c063,  5,   5, 0x80000004L)>]
[<InlineData(0x0220c063, -5,  -5, 0x80000004L)>]
[<InlineData(0x0220c063, -5,  -1, 0x80000020L)>]
[<InlineData(0x0220c063, -5, -10, 0x80000004L)>]
[<InlineData(0xfe20c6e3,  5,  10, 0x7fffffecL)>]
[<InlineData(0xfe20c6e3, -5,  10, 0x7fffffecL)>]
[<InlineData(0xfe20c6e3,  5,  -5, 0x80000004L)>]
[<InlineData(0xfe20c6e3,  5,   5, 0x80000004L)>]
[<InlineData(0xfe20c6e3, -5,  -5, 0x80000004L)>]
[<InlineData(0xfe20c6e3, -5,  -1, 0x7fffffecL)>]
[<InlineData(0xfe20c6e3, -5, -10, 0x80000004L)>]
let ``BLT: x1 < x2`` (instr, x1, x2, addrRes) =
    Branch instr x1 x2 addrRes

[<Theory>]
[<InlineData(0x0020dc63,  5,  10, 0x80000004L)>]
[<InlineData(0x0020dc63, -5,  10, 0x80000004L)>]
[<InlineData(0x0020dc63,  5,  -5, 0x80000018L)>]
[<InlineData(0x0020dc63, 10,   5, 0x80000018L)>]
[<InlineData(0x0020dc63,  5,   5, 0x80000018L)>]
[<InlineData(0x0020dc63, -5,  -5, 0x80000018L)>]
[<InlineData(0x0020dc63, -5,  -1, 0x80000004L)>]
[<InlineData(0x0020dc63, -5, -10, 0x80000018L)>]
[<InlineData(0xfe20d2e3,  5,  10, 0x80000004L)>]
[<InlineData(0xfe20d2e3, -5,  10, 0x80000004L)>]
[<InlineData(0xfe20d2e3,  5,  -5, 0x7fffffe4L)>]
[<InlineData(0xfe20d2e3, 10,   5, 0x7fffffe4L)>]
[<InlineData(0xfe20d2e3,  5,   5, 0x7fffffe4L)>]
[<InlineData(0xfe20d2e3, -5,  -5, 0x7fffffe4L)>]
[<InlineData(0xfe20d2e3, -5,  -1, 0x80000004L)>]
[<InlineData(0xfe20d2e3, -5, -10, 0x7fffffe4L)>]
let ``BGE: x1 >= x2`` (instr, x1, x2, addrRes) =
    Branch instr x1 x2 addrRes

[<Theory>]
[<InlineData(0x0020e863,  5, 10, 0x80000010L)>]
[<InlineData(0x0020e863,  5,  5, 0x80000004L)>]
[<InlineData(0x0020e863,  0, 10, 0x80000010L)>]
[<InlineData(0x0020e863, -1,  5, 0x80000004L)>]
[<InlineData(0x0020e863, 10, -1, 0x80000010L)>]
[<InlineData(0x0020e863, -5, -1, 0x80000010L)>]
[<InlineData(0xfc20eee3, 5, 10, 0x7fffffdcL)>]
[<InlineData(0xfc20eee3, 5,  5, 0x80000004L)>]
[<InlineData(0xfc20eee3,  0, 10, 0x7fffffdcL)>]
[<InlineData(0xfc20eee3, -1,  5, 0x80000004L)>]
[<InlineData(0xfc20eee3, 10, -1, 0x7fffffdcL)>]
[<InlineData(0xfc20eee3, -5, -1, 0x7fffffdcL)>]
let ``BLTU: x1 < x2`` (instr, x1, x2, addrRes) =
    Branch instr x1 x2 addrRes

[<Theory>]
[<InlineData(0x0020f463,  5, 10, 0x80000004L)>]
[<InlineData(0x0020f463, 10,  5, 0x80000008L)>]
[<InlineData(0x0020f463,  5,  5, 0x80000008L)>]
[<InlineData(0x0020f463, -1,  5, 0x80000008L)>]
[<InlineData(0x0020f463, -1, -1, 0x80000008L)>]
[<InlineData(0x0020f463, -1, -2, 0x80000008L)>]
[<InlineData(0xfc20fae3,  5, 10, 0x80000004L)>]
[<InlineData(0xfc20fae3, 10,  5, 0x7fffffd4L)>]
[<InlineData(0xfc20fae3,  5,  5, 0x7fffffd4L)>]
[<InlineData(0xfc20fae3, -1,  5, 0x7fffffd4L)>]
[<InlineData(0xfc20fae3, -1, -1, 0x7fffffd4L)>]
[<InlineData(0xfc20fae3, -1, -2, 0x7fffffd4L)>]
let ``BGEU: x1 >= x2`` (instr, x1, x2, addrRes) =
    Branch instr x1 x2 addrRes
