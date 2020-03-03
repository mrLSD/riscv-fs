module Tests.rv32a.amo

open Xunit

open ISA.RISCV
open ISA.RISCV.Arch
open ISA.RISCV.MachineState

//===============================================
// ALU tests
let ALU (instrs: InstrField array) =
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
    let a0 = m.getRegister 10 
    Assert.Equal(a0, 0xffffffff80000000L)
    let a3 = m.getRegister 13 
    Assert.Equal(a3, 0X80001000L)
    let a4 = m.getRegister 14
    Assert.Equal(a4, 0XFFFFFFFF80000000L)
    

[<Theory>]
[<InlineData()>]
let ``AMO.ADD`` () =
    ALU [|
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
