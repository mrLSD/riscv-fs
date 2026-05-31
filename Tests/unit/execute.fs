module Tests.unit.execute

open Xunit

open ISA.RISCV
open ISA.RISCV.Arch
open ISA.RISCV.MachineState
open ISA.RISCV.Utils.Bits

module DecI = ISA.RISCV.Decode.I
module ExI = ISA.RISCV.Execute.I
module DecI64 = ISA.RISCV.Decode.I64
module ExI64 = ISA.RISCV.Execute.I64
module DecM = ISA.RISCV.Decode.M
module ExM = ISA.RISCV.Execute.M
module DecM64 = ISA.RISCV.Decode.M64
module ExM64 = ISA.RISCV.Execute.M64
module DecA = ISA.RISCV.Decode.A
module ExA = ISA.RISCV.Execute.A
module DecA64 = ISA.RISCV.Decode.A64
module ExA64 = ISA.RISCV.Execute.A64

let private st arch = (MachineState.InitMachineState Map.empty arch false).setRunState RunMachineState.Run
let private step (m : MachineState) instr = (Decoder.Decode m instr).Value m
let private isMemTrap (m : MachineState) = match m.RunState with | Trap (MemAddress _) -> true | _ -> false
let private isTrap (m : MachineState) = match m.RunState with | Trap _ -> true | _ -> false

let private encW funct5 rs2 rs1 rd =
    (funct5 <<< 27) ||| (rs2 <<< 20) ||| (rs1 <<< 15) ||| (0b010 <<< 12) ||| (rd <<< 7) ||| 0b0101111
let private encD funct5 rs2 rs1 rd =
    (funct5 <<< 27) ||| (rs2 <<< 20) ||| (rs1 <<< 15) ||| (0b011 <<< 12) ||| (rd <<< 7) ||| 0b0101111

// AMO/LR funct5 values that load from memory (SC excluded)
let private loadingFunct5 = [ 0b00010; 0b00001; 0b00000; 0b00100; 0b01100; 0b01000; 0b10000; 0b10100; 0b11000; 0b11100 ]

[<Theory>]
[<InlineData(0x00000083)>] // LB  x1,0(x0)
[<InlineData(0x00001083)>] // LH
[<InlineData(0x00002083)>] // LW
[<InlineData(0x00004083)>] // LBU
[<InlineData(0x00005083)>] // LHU
let ``I loads trap on an unmapped address`` (instr : int) =
    Assert.True(isMemTrap (step (st RV32i) instr))

[<Theory>]
[<InlineData(0x00006083)>] // LWU x1,0(x0)
[<InlineData(0x00003083)>] // LD
let ``I64 loads trap on an unmapped address`` (instr : int) =
    Assert.True(isMemTrap (step (st RV64i) instr))

[<Fact>]
let ``JALR misaligned target traps`` () =
    match (step (st RV32i) 0x00200067).RunState with
    | Trap JumpAddress -> () | s -> Assert.True(false, sprintf "%A" s)

[<Fact>]
let ``JALR to self stops`` () =
    let m = (st RV64i).setRegister 1 0x80000000L
    Assert.Equal(RunMachineState.Stopped, (step m 0x00008067).RunState)

[<Fact>]
let ``JAL misaligned target traps`` () =
    match (step (st RV32i) 0x0020006f).RunState with
    | Trap JumpAddress -> () | s -> Assert.True(false, sprintf "%A" s)

[<Fact>]
let ``JAL to self stops`` () =
    Assert.Equal(RunMachineState.Stopped, (step (st RV32i) 0x0000006f).RunState)

[<Theory>]
[<InlineData(0x00000163)>] // beq  x0,x0,2 -> misaligned
[<InlineData(0x00006163)>] // bltu x0,x0,2
[<InlineData(0x00007163)>] // bgeu x0,x0,2
let ``branch misaligned target traps`` (instr : int) =
    match (step (st RV32i) instr).RunState with
    | Trap BreakAddress -> () | s -> Assert.True(false, sprintf "%A" s)

[<Theory>]
[<InlineData(0x00000063)>] // beq  x0,x0,0 -> self
[<InlineData(0x00006063)>] // bltu
[<InlineData(0x00007063)>] // bgeu
let ``branch to self stops`` (instr : int) =
    Assert.Equal(RunMachineState.Stopped, (step (st RV32i) instr).RunState)

[<Fact>]
let ``A .W ops trap on an unmapped aligned address`` () =
    for f5 in loadingFunct5 do
        let m = (st RV64ia).setRegister 1 0x4L
        Assert.True(isMemTrap (step m (encW f5 2 1 3)))

[<Fact>]
let ``A .D ops trap on an unmapped aligned address`` () =
    for f5 in loadingFunct5 do
        let m = (st RV64ia).setRegister 1 0x8L
        Assert.True(isMemTrap (step m (encD f5 2 1 3)))

[<Fact>]
let ``SC.D without a reservation fails`` () =
    let m = (st RV64ia).setRegister 1 0x8L
    Assert.Equal(1L, (step m (encD 0b00011 2 1 3)).getRegister 3)

[<Fact>]
let ``Execute None traps for every instruction set (dead dispatch arms)`` () =
    let m = st RV64ima
    Assert.True(isTrap (ExI.Execute DecI.InstructionI.None m))
    Assert.True(isTrap (ExI64.Execute DecI64.InstructionI64.None m))
    Assert.True(isTrap (ExM.Execute DecM.InstructionM.None m))
    Assert.True(isTrap (ExM64.Execute DecM64.InstructionM64.None m))
    Assert.True(isTrap (ExA.Execute DecA.InstructionA.None m))
    Assert.True(isTrap (ExA64.Execute DecA64.InstructionA64.None m))

// AMO min/max: exercise both selection branches (mem-kept vs rs2-selected).
let private amoMem f5 isD (memVal : int64) (rs2Val : int64) =
    let addr = 0x40L
    let m = (st RV64ia).setRegister 1 addr
    let m = m.setRegister 2 rs2Val
    let m = m.storeMemoryDoubleWord addr memVal
    let m = step m ((if isD then encD else encW) f5 2 1 3)
    if isD then (loadDouble m.Memory addr).Value else int64 (loadWord m.Memory addr).Value

[<Fact>]
let ``AMOMAX.W / AMOMAXU.W keep memory when it is not smaller`` () =
    Assert.Equal(10L, amoMem 0b10100 false 10L 5L)
    Assert.Equal(10L, amoMem 0b11100 false 10L 5L)

[<Fact>]
let ``AMO .D min/max cover both selection branches`` () =
    Assert.Equal(5L,  amoMem 0b10000 true 10L 5L)   // AMOMIN.D  mem>rs2 -> rs2
    Assert.Equal(10L, amoMem 0b10100 true 10L 5L)   // AMOMAX.D  mem>rs2 -> mem
    Assert.Equal(5L,  amoMem 0b11000 true 5L 10L)   // AMOMINU.D mem<rs2 -> mem
    Assert.Equal(10L, amoMem 0b11100 true 5L 10L)   // AMOMAXU.D mem<rs2 -> rs2

[<Fact>]
let ``AMO.W arithmetic uses only low 32 bits of rs2 on RV64`` () =
    Assert.Equal(15L,   amoMem 0b00000 false 10L   0x0000000100000005L)  // AMOADD.W  10 + 5
    Assert.Equal(0xABL, amoMem 0b00001 false 10L   0x00000001000000ABL)  // AMOSWAP.W store 0xAB
    Assert.Equal(0xFL,  amoMem 0b01100 false 0xFFL 0x000000010000000FL)  // AMOAND.W  0xFF & 0xF
