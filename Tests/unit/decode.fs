module Tests.unit.decode

open Xunit

open ISA.RISCV
open ISA.RISCV.Arch
open ISA.RISCV.MachineState

module DecI = ISA.RISCV.Decode.I
module DecI64 = ISA.RISCV.Decode.I64
module DecM = ISA.RISCV.Decode.M
module DecM64 = ISA.RISCV.Decode.M64
module DecA = ISA.RISCV.Decode.A
module DecA64 = ISA.RISCV.Decode.A64

let private m32 = MachineState.InitMachineState Map.empty RV32i false
let private m64 = MachineState.InitMachineState Map.empty RV64ima false

// ---- illegal-instruction (_ -> None) decode arms ----
[<Fact>]
let ``I decode returns None for illegal encodings`` () =
    Assert.Equal(DecI.InstructionI.None, DecI.Decode m32 0x00002063)   // branch funct3=010
    Assert.Equal(DecI.InstructionI.None, DecI.Decode m32 0x40001013)   // imm funct3=001 with bad funct6

[<Fact>]
let ``I64 decode returns None for illegal encoding`` () =
    Assert.Equal(DecI64.InstructionI64.None, DecI64.Decode 0x0000201b) // 0b0011011 funct3=010

[<Fact>]
let ``M64 decode returns None for unsupported funct3`` () =
    Assert.Equal(DecM64.InstructionM64.None, DecM64.Decode m64 0x0200103b) // funct3=001

[<Fact>]
let ``A decode returns None for unknown funct5`` () =
    Assert.Equal(DecA.InstructionA.None, DecA.Decode 0x2800202f)   // funct5=00101, .W

[<Fact>]
let ``A64 decode returns None for unknown funct5`` () =
    Assert.Equal(DecA64.InstructionA64.None, DecA64.Decode 0x2800302f) // funct5=00101, .D

// ---- verbosityMessage (logging) coverage ----
[<Fact>]
let ``I verbosityMessage covers all arms`` () =
    for w in [ 0x000000b7; 0x00000097; 0x0000006f; 0x00000067; 0x00000083; 0x00000063; 0x00001093; 0x000000b3; 0x0000000f ] do
        DecI.verbosityMessage w (DecI.Decode m32 w) m32
    DecI.verbosityMessage 0 DecI.InstructionI.None m32

[<Fact>]
let ``I64 verbosityMessage covers all arms`` () =
    for w in [ 0x00006083; 0x00003023; 0x0000101b; 0x0000003b ] do
        DecI64.verbosityMessage w (DecI64.Decode w) m64
    DecI64.verbosityMessage 0 DecI64.InstructionI64.None m64

[<Fact>]
let ``M verbosityMessage covers all arms`` () =
    DecM.verbosityMessage 0x02000033 (DecM.Decode m64 0x02000033) m64   // MUL
    DecM.verbosityMessage 0 DecM.InstructionM.None m64

[<Fact>]
let ``M64 verbosityMessage covers all arms`` () =
    DecM64.verbosityMessage 0x0200003b (DecM64.Decode m64 0x0200003b) m64 // MULW
    DecM64.verbosityMessage 0 DecM64.InstructionM64.None m64

[<Fact>]
let ``A verbosityMessage covers all arms`` () =
    DecA.verbosityMessage 0x1000202f (DecA.Decode 0x1000202f) m64   // LR.W
    DecA.verbosityMessage 0x0000202f (DecA.Decode 0x0000202f) m64   // AMOADD.W
    DecA.verbosityMessage 0 DecA.InstructionA.None m64

[<Fact>]
let ``A64 verbosityMessage covers all arms`` () =
    DecA64.verbosityMessage 0x1000302f (DecA64.Decode 0x1000302f) m64   // LR.D
    DecA64.verbosityMessage 0x0000302f (DecA64.Decode 0x0000302f) m64   // AMOADD.D
    DecA64.verbosityMessage 0 DecA64.InstructionA64.None m64
