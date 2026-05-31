module Tests.unit.units

open Xunit

open ISA.RISCV
open ISA.RISCV.Arch
open ISA.RISCV.MachineState
open ISA.RISCV.Utils.Bits

[<Theory>]
[<InlineData("rv32i")>]
[<InlineData("rv64i")>]
[<InlineData("rv32im")>]
[<InlineData("rv64im")>]
[<InlineData("rv32ia")>]
[<InlineData("rv64ia")>]
[<InlineData("rv32ima")>]
[<InlineData("rv64ima")>]
let ``Architecture.fromString parses every valid arch`` (s : string) =
    Assert.True((Architecture.fromString s).IsSome)

[<Fact>]
let ``Architecture.fromString rejects an unknown arch`` () =
    Assert.True((Architecture.fromString "rv128gc").IsNone)

[<Fact>]
let ``Int64 bit helpers`` () =
    let x = 0b1010L
    Assert.Equal(0b10L, x.bitSlice 3 2)
    Assert.True(x.isSet 1)
    Assert.False(x.isSet 0)
    Assert.Equal("0xa", x.toHex)
    Assert.Equal(64, x.toBin.Length)
    Assert.Equal<int[]>([| 1; 3 |], x.toArray)
    x.print
    x.display

[<Fact>]
let ``load helpers return None on unmapped memory`` () =
    let mem : Map<int64, byte> = Map.empty
    Assert.True((loadByte mem 0L).IsNone)
    Assert.True((loadHalfWord mem 0L).IsNone)
    Assert.True((loadWord mem 0L).IsNone)
    Assert.True((loadDouble mem 0L).IsNone)

[<Fact>]
let ``getMemory returns Some for mapped and None for unmapped`` () =
    let m = (MachineState.InitMachineState Map.empty RV32i false).setMemoryByte 0x10L 0xABuy
    Assert.Equal(Some 0xABuy, m.getMemory 0x10L)
    Assert.Equal(None, m.getMemory 0x20L)
