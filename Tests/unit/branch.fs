module Tests.unit.branch

open Xunit

open ISA.RISCV
open ISA.RISCV.Arch
open ISA.RISCV.MachineState

module DI   = ISA.RISCV.Decode.I
module DI64 = ISA.RISCV.Decode.I64
module DM   = ISA.RISCV.Decode.M
module DM64 = ISA.RISCV.Decode.M64
module DA   = ISA.RISCV.Decode.A
module DA64 = ISA.RISCV.Decode.A64

let private m32 = MachineState.InitMachineState Map.empty RV32i false
let private m64 = MachineState.InitMachineState Map.empty RV64ima false

// =====================================================================
// verbosityMessage: decode one real encoding per constructor so every
// OR-pattern alternative arm is exercised (full branch coverage).
// (Anonymous records can't cross the assembly boundary, so we decode
//  rather than build the DU values directly.)
// =====================================================================

[<Fact>]
let ``I verbosityMessage: every constructor arm`` () =
    [ 0x000000b7                                                              // LUI
      0x00000097                                                              // AUIPC
      0x0000006f                                                              // JAL
      0x00000067                                                              // JALR
      0x00000063; 0x00001063; 0x00004063; 0x00005063; 0x00006063; 0x00007063 // BEQ BNE BLT BGE BLTU BGEU
      0x00000083; 0x00001083; 0x00002083; 0x00004083; 0x00005083             // LB LH LW LBU LHU
      0x00000023; 0x00001023; 0x00002023                                     // SB SH SW
      0x00000013; 0x00002013; 0x00003013; 0x00004013; 0x00006013; 0x00007013 // ADDI SLTI SLTIU XORI ORI ANDI
      0x00001013; 0x00005013; 0x40005013                                     // SLLI SRLI SRAI
      0x00000033; 0x40000033; 0x00001033; 0x00002033; 0x00003033             // ADD SUB SLL SLT SLTU
      0x00004033; 0x00005033; 0x40005033; 0x00006033; 0x00007033             // XOR SRL SRA OR AND
      0x0000000f; 0x00000073; 0x00100073 ]                                   // FENCE ECALL EBREAK
    |> List.iter (fun w -> DI.verbosityMessage w (DI.Decode m32 w) m32)
    DI.verbosityMessage 0 DI.None m32   // _ -> "Undef"

[<Fact>]
let ``I64 verbosityMessage: every constructor arm`` () =
    [ 0x00006003; 0x00003003; 0x00003023; 0x0000001b                         // LWU LD SD ADDIW
      0x0000101b; 0x0000501b; 0x4000501b                                     // SLLIW SRLIW SRAIW
      0x0000003b; 0x4000003b; 0x0000103b; 0x0000503b; 0x4000503b ]           // ADDW SUBW SLLW SRLW SRAW
    |> List.iter (fun w -> DI64.verbosityMessage w (DI64.Decode w) m64)
    DI64.verbosityMessage 0 DI64.None m64

[<Fact>]
let ``M verbosityMessage: every constructor arm`` () =
    [ 0x02000033; 0x02001033; 0x02002033; 0x02003033                         // MUL MULH MULHSU MULHU
      0x02004033; 0x02005033; 0x02006033; 0x02007033 ]                       // DIV DIVU REM REMU
    |> List.iter (fun w -> DM.verbosityMessage w (DM.Decode m64 w) m64)
    DM.verbosityMessage 0 DM.None m64

[<Fact>]
let ``M64 verbosityMessage: every constructor arm`` () =
    [ 0x0200003b; 0x0200403b; 0x0200503b; 0x0200603b; 0x0200703b ]           // MULW DIVW DIVUW REMW REMUW
    |> List.iter (fun w -> DM64.verbosityMessage w (DM64.Decode m64 w) m64)
    DM64.verbosityMessage 0 DM64.None m64

[<Fact>]
let ``A verbosityMessage: every constructor arm`` () =
    [ 0x1000202f; 0x1800202f; 0x0800202f; 0x0000202f; 0x2000202f; 0x6000202f // LR SC SWAP ADD XOR AND
      0x4000202f; 0x8000202f; 0xA000202f; 0xC000202f; 0xE000202f ]           // OR MIN MAX MINU MAXU
    |> List.iter (fun w -> DA.verbosityMessage w (DA.Decode w) m64)
    DA.verbosityMessage 0 DA.None m64

[<Fact>]
let ``A64 verbosityMessage: every constructor arm`` () =
    [ 0x1000302f; 0x1800302f; 0x0800302f; 0x0000302f; 0x2000302f; 0x6000302f // LR SC SWAP ADD XOR AND
      0x4000302f; 0x8000302f; 0xA000302f; 0xC000302f; 0xE000302f ]           // OR MIN MAX MINU MAXU
    |> List.iter (fun w -> DA64.verbosityMessage w (DA64.Decode w) m64)
    DA64.verbosityMessage 0 DA64.None m64

// =====================================================================
// DecodeI: false-branches of the shift / FENCE / SYSTEM when-guards.
// =====================================================================

[<Fact>]
let ``I decode: SRAI guard false branches`` () =
    // funct3=101, funct6=0b010000, shamt[5]=1 on RV32 -> shamt_ok=false -> None
    Assert.Equal(DI.None, DI.Decode m32 0x42005013)
    // funct3=101 with funct6 neither 0 nor 0b010000 -> guard cond1 false -> None
    Assert.Equal(DI.None, DI.Decode m32 0x80005013)

[<Fact>]
let ``I decode: FENCE/SYSTEM guard false branches`` () =
    // FENCE opcode (0b0001111): rd/rs1 are ignored per spec; only funct3 != 0 -> None
    Assert.Equal(DI.None, DI.Decode m32 0x0000100F)  // funct3=1 (e.g. unimplemented FENCE.I)
    // SYSTEM opcode (0b1110011) with rd / rs1 / funct3 != 0, or imm not 0/1 -> None
    Assert.Equal(DI.None, DI.Decode m32 0x000000F3)  // rd=1
    Assert.Equal(DI.None, DI.Decode m32 0x00008073)  // rs1=1
    Assert.Equal(DI.None, DI.Decode m32 0x00001073)  // funct3=1
    Assert.Equal(DI.None, DI.Decode m32 0x00200073)  // imm12=2 (neither ECALL nor EBREAK)
