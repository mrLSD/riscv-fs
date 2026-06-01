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
[<InlineData("rv32ic")>]
[<InlineData("rv64ic")>]
[<InlineData("rv32imc")>]
[<InlineData("rv64imc")>]
[<InlineData("rv32iac")>]
[<InlineData("rv64iac")>]
[<InlineData("rv32imac")>]
[<InlineData("rv64imac")>]
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
let ``combineBytes over empty, single and multi-byte arrays`` () =
    Assert.Equal(0L, combineBytes [||])                       // empty range branch
    Assert.Equal(0xABL, combineBytes [| 0xABuy |])            // single byte
    Assert.Equal(0xCDABL, combineBytes [| 0xABuy; 0xCDuy |])  // little-endian combine

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

[<Fact>]
let ``setRegister does not mutate the source state`` () =
    let m0 = MachineState.InitMachineState Map.empty RV64i false
    let m1 = m0.setRegister 1 99L
    Assert.Equal(0L, m0.getRegister 1)
    Assert.Equal(99L, m1.getRegister 1)
