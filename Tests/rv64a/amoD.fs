module Tests.rv64a.amoD

open Xunit

open ISA.RISCV
open ISA.RISCV.Arch
open ISA.RISCV.MachineState
open ISA.RISCV.Utils.Bits

// Encode an A-extension `.D` instruction (funct3 = 0b011, aq = rl = 0)
// with rd = x12, rs1 = x10 (address), rs2 = x11 (operand).
// Distinct rs1/rs2 so a regression to rs2 = rs1 is caught.
let private encodeD funct5 =
    (funct5 <<< 27) ||| (11 <<< 20) ||| (10 <<< 15) ||| (0b011 <<< 12) ||| (12 <<< 7) ||| 0b0101111

// Decode + execute one `.D` atomic against memory[addr] = memVal, x11 = rs2Val.
// Returns (x12, memory-after) for assertions.
let private runD funct5 (memVal : int64) (rs2Val : int64) =
    let addr = 0x1000L
    let mstate = MachineState.InitMachineState Map.empty RV64ia false
    let mstate = mstate.setPC 0x80000000L
    let mstate = mstate.setRunState RunMachineState.Run
    let mstate = mstate.setRegister 10 addr
    let mstate = mstate.setRegister 11 rs2Val
    let mstate = mstate.storeMemoryDoubleWord addr memVal
    let executor = Decoder.Decode mstate (encodeD funct5)
    Assert.NotEqual(executor, None)
    let mstate = executor.Value mstate
    Assert.Equal(0x80000004L, mstate.PC)
    (mstate.getRegister 12, (loadDouble mstate.Memory addr).Value)

[<Fact>]
let ``LR.D loads the doubleword`` () =
    let (rd, mem) = runD 0b00010 0x1122334455667788L 0xDEADL
    Assert.Equal(0x1122334455667788L, rd)
    Assert.Equal(0x1122334455667788L, mem)

[<Fact>]
let ``SC.D after LR.D stores rs2 and writes 0 to rd`` () =
    let addr = 0x1000L
    let mstate = MachineState.InitMachineState Map.empty RV64ia false
    let mstate = mstate.setPC 0x80000000L
    let mstate = mstate.setRunState RunMachineState.Run
    let mstate = mstate.setRegister 10 addr
    let mstate = mstate.setRegister 11 0x2222L
    let mstate = mstate.storeMemoryDoubleWord addr 0x1111L
    let mstate = (Decoder.Decode mstate (encodeD 0b00010)).Value mstate
    let mstate = (Decoder.Decode mstate (encodeD 0b00011)).Value mstate
    Assert.Equal(0L, mstate.getRegister 12)
    Assert.Equal(0x2222L, (loadDouble mstate.Memory addr).Value)

[<Fact>]
let ``AMOSWAP.D swaps memory and rs2`` () =
    let (rd, mem) = runD 0b00001 100L 7L
    Assert.Equal(100L, rd)
    Assert.Equal(7L, mem)

[<Fact>]
let ``AMOADD.D adds rs2 to memory`` () =
    let (rd, mem) = runD 0b00000 100L 7L
    Assert.Equal(100L, rd)
    Assert.Equal(107L, mem)

[<Fact>]
let ``AMOXOR.D`` () =
    let (rd, mem) = runD 0b00100 0b1100L 0b1010L
    Assert.Equal(0b1100L, rd)
    Assert.Equal(0b0110L, mem)

[<Fact>]
let ``AMOAND.D`` () =
    let (rd, mem) = runD 0b01100 0b1100L 0b1010L
    Assert.Equal(0b1100L, rd)
    Assert.Equal(0b1000L, mem)

[<Fact>]
let ``AMOOR.D`` () =
    let (rd, mem) = runD 0b01000 0b1100L 0b1010L
    Assert.Equal(0b1100L, rd)
    Assert.Equal(0b1110L, mem)

[<Fact>]
let ``AMOMIN.D signed`` () =
    let (rd, mem) = runD 0b10000 -1L 1L
    Assert.Equal(-1L, rd)
    Assert.Equal(-1L, mem)

[<Fact>]
let ``AMOMAX.D signed`` () =
    let (rd, mem) = runD 0b10100 -1L 1L
    Assert.Equal(-1L, rd)
    Assert.Equal(1L, mem)

[<Fact>]
let ``AMOMINU.D unsigned`` () =
    let (rd, mem) = runD 0b11000 -1L 1L
    Assert.Equal(-1L, rd)
    Assert.Equal(1L, mem)

[<Fact>]
let ``AMOMAXU.D unsigned`` () =
    let (rd, mem) = runD 0b11100 -1L 1L
    Assert.Equal(-1L, rd)
    Assert.Equal(-1L, mem)
