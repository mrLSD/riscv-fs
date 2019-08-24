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
    let instr = 0x00300113L
    let addr = 0x80000000L
    let mstate = MachineState.InitMachineState Map.empty RV32i true
    let mstate = mstate.setPC 0x80000000L
    let decodedInstr = I.DecodeI instr
    Assert.NotEqual(decodedInstr, I.None)
    let mstate = ExecuteI.ExecuteI decodedInstr mstate
    Assert.Equal(0x80000004L, mstate.PC)
