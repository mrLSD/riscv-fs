module Tests

open Microsoft.FSharp.Collections
open Xunit

open ISA.RISCV
open ISA.RISCV
open ISA.RISCV.Utils.Bits
open ISA.RISCV.Arch
open ISA.RISCV.Decode

[<Fact>]
let ``ADDI to zero`` () =
    // Init MachineState
    let addr = 0x80000000L
    let mstate = MachineState.InitMachineState Map.empty RV32i true
    let mstate = mstate.setPC 0x80000000L

    // x2 = x0 + 3
    let instr = 0x00300113L
    let decodedInstr = I.DecodeI instr
    Assert.NotEqual(decodedInstr, I.None)
    let mstate = ExecuteI.ExecuteI decodedInstr mstate

    // x3 = x2 + 5
    let instr = 0x00510193L
    let decodedInstr = I.DecodeI instr
    Assert.NotEqual(decodedInstr, I.None)
    let mstate = ExecuteI.ExecuteI decodedInstr mstate

    Assert.Equal(mstate.getRegister 2, 3L)
    Assert.Equal(mstate.getRegister 3, 8L)

    Assert.Equal(addr + 8L, mstate.PC)
