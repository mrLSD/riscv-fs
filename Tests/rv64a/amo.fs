module Tests.rv32a.amo

open Xunit

open ISA.RISCV
open ISA.RISCV.Arch
open ISA.RISCV.MachineState

//===============================================
// ALU tests
let ALU (instrs: InstrField array) (a4 : int64) (a5 : int64)  (a6 : int64)  (a7 : int64)=
    // Init MachineState
    let addr = 0x80000000L
    let mstate = MachineState.InitMachineState Map.empty RV64ia true
    let mstate = mstate.setPC addr
    let mstate = mstate.setRunState RunMachineState.Run 

    let m = Array.fold (fun (m : MachineState) i ->
                let pc = m.PC
                let executor = Decoder.Decode m i
                Assert.NotEqual(executor, None)
                let m = executor.Value m
                Assert.Equal(m.RunState, RunMachineState.Run)
                Assert.Equal(pc + 4L, m.PC)
                m
            ) mstate instrs
    let x14 = m.getRegister 14
    let x15 = m.getRegister 15
    let x16 = m.getRegister 16
    let x17 = m.getRegister 17
    
    Assert.Equal(x14, a4)
    Assert.Equal(x15, a5)
    Assert.Equal(x16, a6)
    Assert.Equal(x17, a7)
    

[<Theory>]
[<InlineData(0xffffffff80000000L, 0x000000007ffff800L, 0x000000007ffff800L, 0xfffffffffffff800L)>]
let ``AMO.ADD`` (a4, a5, a6, a7) =
    let instrSet = [|
            0x80000537
            0x80000593
            0x00001697
            0xff868693
            0x00a6a023
            0x00b6a72f
            0x0006a783
            0x800005b7
            0x00b6a82f
            0x0006a883
        |]
    ALU instrSet a4 a5
