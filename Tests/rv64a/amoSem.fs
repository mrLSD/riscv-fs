module Tests.rv64a.amoSem

open Xunit

open ISA.RISCV
open ISA.RISCV.Arch
open ISA.RISCV.MachineState
open ISA.RISCV.Utils.Bits

// A-extension encoders (aq = rl = 0); funct3 = 0b010 for .W, 0b011 for .D.
let private encW funct5 rs2 rs1 rd =
    (funct5 <<< 27) ||| (rs2 <<< 20) ||| (rs1 <<< 15) ||| (0b010 <<< 12) ||| (rd <<< 7) ||| 0b0101111
let private encD funct5 rs2 rs1 rd =
    (funct5 <<< 27) ||| (rs2 <<< 20) ||| (rs1 <<< 15) ||| (0b011 <<< 12) ||| (rd <<< 7) ||| 0b0101111

let private init () =
    (MachineState.InitMachineState Map.empty RV64ia false).setRunState RunMachineState.Run

let private exec (m : MachineState) (instr : int) =
    let e = Decoder.Decode m instr
    Assert.NotEqual(e, None)
    e.Value m

// ---- LR/SC reservation ----
[<Fact>]
let ``SC.W succeeds after LR.W to the same address`` () =
    let addr = 0x2000L
    let m = init ()
    let m = m.setRegister 10 addr
    let m = m.setRegister 11 0x1234L
    let m = m.storeMemoryWord addr 0L
    let m = exec m (encW 0b00010 0 10 0)
    let m = exec m (encW 0b00011 11 10 12)
    Assert.Equal(0L, m.getRegister 12)
    Assert.Equal(0x1234L, int64 (loadWord m.Memory addr).Value)

[<Fact>]
let ``SC.W fails without a prior LR.W`` () =
    let addr = 0x2000L
    let m = init ()
    let m = m.setRegister 10 addr
    let m = m.setRegister 11 0x1234L
    let m = m.storeMemoryWord addr 0xAAL
    let m = exec m (encW 0b00011 11 10 12)
    Assert.Equal(1L, m.getRegister 12)
    Assert.Equal(0xAAL, int64 (loadWord m.Memory addr).Value)

[<Fact>]
let ``SC.W fails when the address differs from the reservation`` () =
    let m = init ()
    let m = m.setRegister 10 0x2000L
    let m = m.setRegister 9 0x3000L
    let m = m.setRegister 11 0x1234L
    let m = m.storeMemoryWord 0x3000L 0x55L
    let m = exec m (encW 0b00010 0 10 0)
    let m = exec m (encW 0b00011 11 9 12)
    Assert.Equal(1L, m.getRegister 12)
    Assert.Equal(0x55L, int64 (loadWord m.Memory 0x3000L).Value)

// ---- SC must fail when its width does not match the LR it pairs with ----
[<Fact>]
let ``SC.D after LR.W fails on a width mismatch`` () =
    let addr = 0x2000L
    let m = init ()
    let m = m.setRegister 10 addr
    let m = m.setRegister 11 0x1234L
    let m = m.storeMemoryDoubleWord addr 0xAAL
    let m = exec m (encW 0b00010 0 10 0)    // lr.w (width 4) reserves addr
    let m = exec m (encD 0b00011 11 10 12)  // sc.d (width 8) at addr -> must fail
    Assert.Equal(1L, m.getRegister 12)
    Assert.Equal(0xAAL, (loadDouble m.Memory addr).Value)

[<Fact>]
let ``SC.W after LR.D fails on a width mismatch`` () =
    let addr = 0x2000L
    let m = init ()
    let m = m.setRegister 10 addr
    let m = m.setRegister 11 0x1234L
    let m = m.storeMemoryDoubleWord addr 0xAAL
    let m = exec m (encD 0b00010 0 10 0)    // lr.d (width 8) reserves addr
    let m = exec m (encW 0b00011 11 10 12)  // sc.w (width 4) at addr -> must fail
    Assert.Equal(1L, m.getRegister 12)
    Assert.Equal(0xAAL, (loadDouble m.Memory addr).Value)

// ---- AMO.W uses only the low 32 bits of rs2 on RV64 ----
[<Fact>]
let ``AMOMIN.W compares only low 32 bits of rs2`` () =
    let addr = 0x2000L
    let m = init ()
    let m = m.setRegister 10 addr
    let m = m.setRegister 11 0x0000000100000005L
    let m = m.storeMemoryWord addr 10L
    let m = exec m (encW 0b10000 11 10 12)
    Assert.Equal(10L, m.getRegister 12)
    Assert.Equal(5L, int64 (loadWord m.Memory addr).Value)

[<Fact>]
let ``AMOMINU.W compares only low 32 bits of rs2`` () =
    let addr = 0x2000L
    let m = init ()
    let m = m.setRegister 10 addr
    let m = m.setRegister 11 0x0000000100000005L
    let m = m.storeMemoryWord addr 10L
    let m = exec m (encW 0b11000 11 10 12)
    Assert.Equal(10L, m.getRegister 12)
    Assert.Equal(5L, int64 (loadWord m.Memory addr).Value)

// ---- misaligned atomic address traps ----
[<Fact>]
let ``AMOADD.W traps on a misaligned address`` () =
    let m = init ()
    let m = m.setRegister 10 0x2002L
    let m = m.setRegister 11 1L
    let m = m.storeMemoryWord 0x2000L 0L
    let m = m.storeMemoryWord 0x2004L 0L
    let m = exec m (encW 0b00000 11 10 12)
    match m.RunState with
    | Trap (MemAddress _) -> ()
    | s -> Assert.True(false, sprintf "expected misalign trap, got %A" s)

[<Fact>]
let ``AMOADD.D traps on a misaligned address`` () =
    let m = init ()
    let m = m.setRegister 10 0x2004L
    let m = m.setRegister 11 1L
    let m = m.storeMemoryDoubleWord 0x2000L 0L
    let m = m.storeMemoryDoubleWord 0x2008L 0L
    let m = exec m (encD 0b00000 11 10 12)
    match m.RunState with
    | Trap (MemAddress _) -> ()
    | s -> Assert.True(false, sprintf "expected misalign trap, got %A" s)
