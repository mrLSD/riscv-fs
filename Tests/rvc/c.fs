module Tests.rvc.c

open Xunit

open ISA.RISCV
open ISA.RISCV.Arch
open ISA.RISCV.MachineState
open ISA.RISCV.Utils.Bits

module DC = ISA.RISCV.Decode.C
module EC = ISA.RISCV.Execute.C

// ---- 16-bit C instruction encoders (spec bit layout; anchored by the canonical test) ----
let private bit v pos = (v &&& 1) <<< pos
let private fld v hi lo = (v &&& ((1 <<< (hi-lo+1)) - 1)) <<< lo
let private cc r = r - 8   // wide reg (x8..x15) -> 3-bit compressed field
let private cjEnc f3 imm =
    fld f3 15 13 ||| bit (imm>>>11) 12 ||| bit (imm>>>4) 11 ||| fld ((imm>>>8)&&&0x3) 10 9 ||| bit (imm>>>10) 8
    ||| bit (imm>>>6) 7 ||| bit (imm>>>7) 6 ||| fld ((imm>>>1)&&&0x7) 5 3 ||| bit (imm>>>5) 2 ||| 0b01
let private cADDI4SPN rd imm = fld 0b000 15 13 ||| fld ((imm>>>6)&&&0xf) 10 7 ||| fld ((imm>>>4)&&&0x3) 12 11 ||| bit (imm>>>3) 5 ||| bit (imm>>>2) 6 ||| fld (cc rd) 4 2
let private cLW rd rs1 imm = fld 0b010 15 13 ||| bit (imm>>>6) 5 ||| fld ((imm>>>3)&&&0x7) 12 10 ||| bit (imm>>>2) 6 ||| fld (cc rs1) 9 7 ||| fld (cc rd) 4 2
let private cSW rs1 rs2 imm = fld 0b110 15 13 ||| bit (imm>>>6) 5 ||| fld ((imm>>>3)&&&0x7) 12 10 ||| bit (imm>>>2) 6 ||| fld (cc rs1) 9 7 ||| fld (cc rs2) 4 2
let private cLD rd rs1 imm = fld 0b011 15 13 ||| fld ((imm>>>6)&&&0x3) 6 5 ||| fld ((imm>>>3)&&&0x7) 12 10 ||| fld (cc rs1) 9 7 ||| fld (cc rd) 4 2
let private cSD rs1 rs2 imm = fld 0b111 15 13 ||| fld ((imm>>>6)&&&0x3) 6 5 ||| fld ((imm>>>3)&&&0x7) 12 10 ||| fld (cc rs1) 9 7 ||| fld (cc rs2) 4 2
let private cADDI rd imm = fld 0b000 15 13 ||| bit (imm>>>5) 12 ||| fld rd 11 7 ||| fld (imm&&&0x1f) 6 2 ||| 0b01
let private cADDIW rd imm = fld 0b001 15 13 ||| bit (imm>>>5) 12 ||| fld rd 11 7 ||| fld (imm&&&0x1f) 6 2 ||| 0b01
let private cJAL imm = cjEnc 0b001 imm
let private cLI rd imm = fld 0b010 15 13 ||| bit (imm>>>5) 12 ||| fld rd 11 7 ||| fld (imm&&&0x1f) 6 2 ||| 0b01
let private cLUI rd n6 = fld 0b011 15 13 ||| bit (n6>>>5) 12 ||| fld rd 11 7 ||| fld (n6&&&0x1f) 6 2 ||| 0b01
let private cADDI16SP imm = fld 0b011 15 13 ||| bit (imm>>>9) 12 ||| fld 2 11 7 ||| fld ((imm>>>7)&&&0x3) 4 3 ||| bit (imm>>>6) 5 ||| bit (imm>>>5) 2 ||| bit (imm>>>4) 6 ||| 0b01
let private cSRLI rd shamt = fld 0b100 15 13 ||| bit (shamt>>>5) 12 ||| fld 0b00 11 10 ||| fld (cc rd) 9 7 ||| fld (shamt&&&0x1f) 6 2 ||| 0b01
let private cSRAI rd shamt = fld 0b100 15 13 ||| bit (shamt>>>5) 12 ||| fld 0b01 11 10 ||| fld (cc rd) 9 7 ||| fld (shamt&&&0x1f) 6 2 ||| 0b01
let private cANDI rd imm = fld 0b100 15 13 ||| bit (imm>>>5) 12 ||| fld 0b10 11 10 ||| fld (cc rd) 9 7 ||| fld (imm&&&0x1f) 6 2 ||| 0b01
let private caEnc b12 sub rd rs2 = fld 0b100 15 13 ||| (b12<<<12) ||| fld 0b11 11 10 ||| fld (cc rd) 9 7 ||| fld sub 6 5 ||| fld (cc rs2) 4 2 ||| 0b01
let private cJ imm = cjEnc 0b101 imm
let private cBEQZ rs1 imm = fld 0b110 15 13 ||| bit (imm>>>8) 12 ||| fld ((imm>>>3)&&&0x3) 11 10 ||| fld (cc rs1) 9 7 ||| fld ((imm>>>6)&&&0x3) 6 5 ||| fld ((imm>>>1)&&&0x3) 4 3 ||| bit (imm>>>5) 2 ||| 0b01
let private cBNEZ rs1 imm = fld 0b111 15 13 ||| bit (imm>>>8) 12 ||| fld ((imm>>>3)&&&0x3) 11 10 ||| fld (cc rs1) 9 7 ||| fld ((imm>>>6)&&&0x3) 6 5 ||| fld ((imm>>>1)&&&0x3) 4 3 ||| bit (imm>>>5) 2 ||| 0b01
let private cSLLI rd shamt = fld 0b000 15 13 ||| bit (shamt>>>5) 12 ||| fld rd 11 7 ||| fld (shamt&&&0x1f) 6 2 ||| 0b10
let private cLWSP rd imm = fld 0b010 15 13 ||| fld ((imm>>>6)&&&0x3) 3 2 ||| bit (imm>>>5) 12 ||| fld ((imm>>>2)&&&0x7) 6 4 ||| fld rd 11 7 ||| 0b10
let private cLDSP rd imm = fld 0b011 15 13 ||| fld ((imm>>>6)&&&0x7) 4 2 ||| bit (imm>>>5) 12 ||| fld ((imm>>>3)&&&0x3) 6 5 ||| fld rd 11 7 ||| 0b10
let private crEnc b12 rd rs2 = fld 0b100 15 13 ||| (b12<<<12) ||| fld rd 11 7 ||| fld rs2 6 2 ||| 0b10
let private cSWSP rs2 imm = fld 0b110 15 13 ||| fld ((imm>>>6)&&&0x3) 8 7 ||| fld ((imm>>>2)&&&0xf) 12 9 ||| fld rs2 6 2 ||| 0b10
let private cSDSP rs2 imm = fld 0b111 15 13 ||| fld ((imm>>>6)&&&0x7) 9 7 ||| fld ((imm>>>3)&&&0x7) 12 10 ||| fld rs2 6 2 ||| 0b10

// Anchor: encoders reproduce well-known real encodings, validating the bit layout.
[<Fact>]
let ``encoders match canonical hex`` () =
    Assert.Equal(0x0001, cADDI 0 0)      // c.nop
    Assert.Equal(0x0085, cADDI 1 1)
    Assert.Equal(0x4085, cLI 1 1)
    Assert.Equal(0x8082, crEnc 0 1 0)    // c.jr x1 (ret)
    Assert.Equal(0x808a, crEnc 0 1 2)    // c.mv x1,x2
    Assert.Equal(0x908a, crEnc 1 1 2)    // c.add x1,x2
    Assert.Equal(0x9002, crEnc 1 0 0)    // c.ebreak
    Assert.Equal(0x9082, crEnc 1 1 0)    // c.jalr x1
    Assert.Equal(0x4082, cLWSP 1 0)
    Assert.Equal(0xc006, cSWSP 1 0)

// No manual InstrLen: Decoder.Decode derives it from inst[1:0] and bakes it into
// the executor, so a compressed op advances PC by 2 without the caller setting it.
let private st (arch : Architecture) = MachineState.InitMachineState Map.empty arch false
let private run (m : MachineState) (instr : int) =
    let e = Decoder.Decode m instr
    Assert.NotEqual(e, None)
    e.Value m

// ---- Quadrant 0 ----
[<Fact>]
let ``C.ADDI4SPN`` () =
    let m = run ((st RV32ic).setRegister 2 0x1000L) (cADDI4SPN 8 16)
    Assert.Equal(0x1010L, m.getRegister 8)
    Assert.Equal(0x80000002L, m.PC)

[<Fact>]
let ``C.LW`` () =
    let m = ((st RV32ic).setRegister 8 0x2000L).storeMemoryWord 0x2004L 0xABCDL
    let m = run m (cLW 9 8 4)
    Assert.Equal(0xABCDL, int64 (loadWord m.Memory 0x2004L).Value)
    Assert.Equal(0xABCDL, m.getRegister 9)

[<Fact>]
let ``C.SW`` () =
    let m = ((st RV32ic).setRegister 8 0x2000L).setRegister 9 0x1234L
    let m = run m (cSW 8 9 4)
    Assert.Equal(0x1234L, int64 (loadWord m.Memory 0x2004L).Value)

[<Fact>]
let ``C.LD (RV64)`` () =
    let m = ((st RV64ic).setRegister 8 0x2000L).storeMemoryDoubleWord 0x2008L 0x1122334455667788L
    let m = run m (cLD 9 8 8)
    Assert.Equal(0x1122334455667788L, m.getRegister 9)

[<Fact>]
let ``C.SD (RV64)`` () =
    let m = ((st RV64ic).setRegister 8 0x2000L).setRegister 9 0xDEADBEEFCAFEL
    let m = run m (cSD 8 9 8)
    Assert.Equal(0xDEADBEEFCAFEL, (loadDouble m.Memory 0x2008L).Value)

// ---- Quadrant 1 ----
[<Fact>]
let ``C.ADDI`` () =
    let m = run ((st RV32ic).setRegister 5 10L) (cADDI 5 -3)
    Assert.Equal(7L, m.getRegister 5)
    Assert.Equal(0x80000002L, m.PC)

[<Fact>]
let ``C.NOP`` () =
    let m = run (st RV32ic) (cADDI 0 0)
    Assert.Equal(0L, m.getRegister 0)
    Assert.Equal(0x80000002L, m.PC)

[<Fact>]
let ``C.JAL (RV32) links PC+2`` () =
    let m = run ((st RV32ic).setPC 0x1000L) (cJAL 16)
    Assert.Equal(0x1010L, m.PC)
    Assert.Equal(0x1002L, m.getRegister 1)

[<Fact>]
let ``C.ADDIW (RV64)`` () =
    let m = run ((st RV64ic).setRegister 5 0xFFFFFFFFL) (cADDIW 5 1)
    Assert.Equal(0L, m.getRegister 5)

[<Fact>]
let ``C.ADDIW rd, 0 acts as sext.w (RV64)`` () =
    // imm=0 is VALID for C.ADDIW (not reserved): addiw rd,rd,0 == sext.w rd.
    // Upper 32 bits are discarded; bit 31 is sign-extended through bits 63:32.
    let neg = run ((st RV64ic).setRegister 5 0x1234567880000000L) (cADDIW 5 0)
    Assert.Equal(-2147483648L, neg.getRegister 5)   // 0xFFFFFFFF_80000000
    let pos = run ((st RV64ic).setRegister 6 0x1234567800000123L) (cADDIW 6 0)
    Assert.Equal(0x123L, pos.getRegister 6)          // 0x00000000_00000123

[<Fact>]
let ``C.LI`` () =
    let m = run (st RV32ic) (cLI 5 -1)
    Assert.Equal(-1L, m.getRegister 5)

[<Fact>]
let ``C.ADDI16SP`` () =
    let m = run ((st RV32ic).setRegister 2 0x1000L) (cADDI16SP 32)
    Assert.Equal(0x1020L, m.getRegister 2)

[<Fact>]
let ``C.LUI`` () =
    let m = run (st RV32ic) (cLUI 5 1)
    Assert.Equal(0x1000L, m.getRegister 5)

[<Fact>]
let ``C.LUI rd=x0 is a HINT (no-op, not illegal)`` () =
    // rd=x0, nzimm!=0 is a HINT: must decode (not trap) and run as a no-op.
    // PC+2 also confirms InstrLen is derived by Decode (st no longer sets it).
    let m = run (st RV32ic) (cLUI 0 1)
    Assert.Equal(0L, m.getRegister 0)
    Assert.Equal(0x80000002L, m.PC)

[<Fact>]
let ``C.SRLI`` () =
    let m = run ((st RV32ic).setRegister 8 0xF0L) (cSRLI 8 4)
    Assert.Equal(0xFL, m.getRegister 8)

[<Fact>]
let ``C.SRAI`` () =
    let m = run ((st RV32ic).setRegister 8 -16L) (cSRAI 8 2)
    Assert.Equal(-4L, m.getRegister 8)

[<Fact>]
let ``C.ANDI`` () =
    let m = run ((st RV32ic).setRegister 8 0xFFL) (cANDI 8 0xF)
    Assert.Equal(0xFL, m.getRegister 8)

[<Fact>]
let ``C.SUB`` () =
    let m = run (((st RV32ic).setRegister 8 10L).setRegister 9 3L) (caEnc 0 0b00 8 9)
    Assert.Equal(7L, m.getRegister 8)

[<Fact>]
let ``C.XOR`` () =
    let m = run (((st RV32ic).setRegister 8 0b1100L).setRegister 9 0b1010L) (caEnc 0 0b01 8 9)
    Assert.Equal(0b0110L, m.getRegister 8)

[<Fact>]
let ``C.OR`` () =
    let m = run (((st RV32ic).setRegister 8 0b1100L).setRegister 9 0b1010L) (caEnc 0 0b10 8 9)
    Assert.Equal(0b1110L, m.getRegister 8)

[<Fact>]
let ``C.AND`` () =
    let m = run (((st RV32ic).setRegister 8 0b1100L).setRegister 9 0b1010L) (caEnc 0 0b11 8 9)
    Assert.Equal(0b1000L, m.getRegister 8)

[<Fact>]
let ``C.SUBW (RV64)`` () =
    let m = run (((st RV64ic).setRegister 8 10L).setRegister 9 3L) (caEnc 1 0b00 8 9)
    Assert.Equal(7L, m.getRegister 8)

[<Fact>]
let ``C.ADDW (RV64)`` () =
    let m = run (((st RV64ic).setRegister 8 10L).setRegister 9 3L) (caEnc 1 0b01 8 9)
    Assert.Equal(13L, m.getRegister 8)

[<Fact>]
let ``C.J`` () =
    Assert.Equal(0x80000010L, (run (st RV32ic) (cJ 16)).PC)

[<Fact>]
let ``C.J to a 2-byte-aligned target (IALIGN=2)`` () =
    Assert.Equal(0x80000012L, (run (st RV32ic) (cJ 18)).PC)

[<Fact>]
let ``C.BEQZ taken / not taken`` () =
    Assert.Equal(0x80000010L, (run ((st RV32ic).setRegister 8 0L) (cBEQZ 8 16)).PC)
    Assert.Equal(0x80000002L, (run ((st RV32ic).setRegister 8 5L) (cBEQZ 8 16)).PC)

[<Fact>]
let ``C.BNEZ taken / not taken`` () =
    Assert.Equal(0x80000010L, (run ((st RV32ic).setRegister 8 5L) (cBNEZ 8 16)).PC)
    Assert.Equal(0x80000002L, (run ((st RV32ic).setRegister 8 0L) (cBNEZ 8 16)).PC)

// ---- Quadrant 2 ----
[<Fact>]
let ``C.SLLI`` () =
    Assert.Equal(16L, (run ((st RV32ic).setRegister 5 1L) (cSLLI 5 4)).getRegister 5)

[<Fact>]
let ``C.SLLI shamt 32 (RV64)`` () =
    Assert.Equal(0x100000000L, (run ((st RV64ic).setRegister 5 1L) (cSLLI 5 32)).getRegister 5)

[<Fact>]
let ``C.LWSP`` () =
    let m = ((st RV32ic).setRegister 2 0x3000L).storeMemoryWord 0x3004L 0x55L
    Assert.Equal(0x55L, (run m (cLWSP 5 4)).getRegister 5)

[<Fact>]
let ``C.LDSP (RV64)`` () =
    let m = ((st RV64ic).setRegister 2 0x3000L).storeMemoryDoubleWord 0x3008L 0x99L
    Assert.Equal(0x99L, (run m (cLDSP 5 8)).getRegister 5)

[<Fact>]
let ``C.JR`` () =
    let m = run ((st RV32ic).setRegister 5 0x80001000L) (crEnc 0 5 0)
    Assert.Equal(0x80001000L, m.PC)

[<Fact>]
let ``C.MV`` () =
    Assert.Equal(42L, (run ((st RV32ic).setRegister 6 42L) (crEnc 0 5 6)).getRegister 5)

[<Fact>]
let ``C.EBREAK`` () =
    match (run (st RV32ic) (crEnc 1 0 0)).RunState with
    | Trap EBreak -> () | s -> Assert.True(false, sprintf "%A" s)

[<Fact>]
let ``C.JALR links PC+2`` () =
    let m = run (((st RV32ic).setPC 0x1000L).setRegister 5 0x2000L) (crEnc 1 5 0)
    Assert.Equal(0x2000L, m.PC)
    Assert.Equal(0x1002L, m.getRegister 1)

[<Fact>]
let ``C.ADD`` () =
    let m = run (((st RV32ic).setRegister 5 10L).setRegister 6 5L) (crEnc 1 5 6)
    Assert.Equal(15L, m.getRegister 5)

[<Fact>]
let ``C.SWSP`` () =
    let m = run (((st RV32ic).setRegister 2 0x4000L).setRegister 5 0x77L) (cSWSP 5 4)
    Assert.Equal(0x77L, int64 (loadWord m.Memory 0x4004L).Value)

[<Fact>]
let ``C.SDSP (RV64)`` () =
    let m = run (((st RV64ic).setRegister 2 0x4000L).setRegister 5 0x88L) (cSDSP 5 8)
    Assert.Equal(0x88L, (loadDouble m.Memory 0x4008L).Value)

// ---- illegal / reserved encodings decode to None ----
[<Fact>]
let ``reserved and FP-compressed encodings decode to None`` () =
    let m32 = st RV32ic
    let m64 = st RV64ic
    Assert.Equal(DC.None, DC.Decode m32 0x0000)             // all-zero (illegal)
    Assert.Equal(DC.None, DC.Decode m32 (cADDI4SPN 8 0))    // C.ADDI4SPN nzuimm=0
    Assert.Equal(DC.None, DC.Decode m32 0x2000)             // Q0 funct3=001 (C.FLD, needs D)
    Assert.Equal(DC.None, DC.Decode m32 0x6000)             // RV32 Q0 funct3=011 (C.FLW, needs F)
    Assert.Equal(DC.None, DC.Decode m32 0x2002)             // Q2 funct3=001 (C.FLDSP, needs D)
    Assert.Equal(DC.None, DC.Decode m32 (caEnc 1 0b00 8 9)) // RV32 reserved CA (bit12=1)
    Assert.Equal(DC.None, DC.Decode m64 (caEnc 1 0b10 8 9)) // RV64 reserved CA (sub=10)
    Assert.Equal(DC.None, DC.Decode m32 (cSLLI 5 32))       // RV32 C.SLLI shamt>=32
    Assert.Equal(DC.None, DC.Decode m32 (cLWSP 0 4))        // C.LWSP rd=0
    Assert.Equal(DC.None, DC.Decode m32 (crEnc 0 0 0))      // C.JR rd=0
    Assert.Equal(DC.None, DC.Decode m32 (cADDI16SP 0))      // C.ADDI16SP nzimm=0
    Assert.Equal(DC.None, DC.Decode m32 (cLUI 5 0))         // C.LUI nzimm=0
    Assert.Equal(DC.None, DC.Decode m64 (cADDIW 0 1))       // C.ADDIW rd=0
    Assert.Equal(DC.None, DC.Decode m32 (cSRLI 8 32))       // C.SRLI shamt>=32 RV32 (shamtOk=false)
    Assert.Equal(DC.None, DC.Decode m32 (cSRAI 8 32))       // C.SRAI shamt>=32 RV32 (shamtOk=false)
    Assert.Equal(DC.None, DC.Decode m64 (cLDSP 0 8))        // C.LDSP rd=0

[<Fact>]
let ``C Execute None traps`` () =
    match (EC.Execute DC.None (st RV32ic)).RunState with
    | Trap InstructionExecute -> () | s -> Assert.True(false, sprintf "%A" s)

// ---- fetch/PC integration: a compressed program runs through runSteps ----
[<Fact>]
let ``runSteps executes a compressed program (PC += 2)`` () =
    // c.li x5,5 ; c.li x6,3 ; c.add x5,x6 ; c.ebreak
    let prog = [ cLI 5 5; cLI 6 3; crEnc 1 5 6; crEnc 1 0 0 ]
    let m = (MachineState.InitMachineState Map.empty RV32ic false).setRunState RunMachineState.Run
    let m = prog |> List.mapi (fun i w -> (0x80000000L + int64 (i * 2), w))
                 |> List.fold (fun (s : MachineState) (a, w) -> s.storeMemoryHalfWord a (int64 w)) m
    let m = Run.runSteps 10 m
    Assert.Equal(8L, m.getRegister 5)
    match m.RunState with
    | Trap EBreak -> () | s -> Assert.True(false, sprintf "%A" s)

// ---- verbosityMessage: cover every output group ----
[<Fact>]
let ``C verbosityMessage covers every constructor`` () =
    let m = st RV64ic
    let vm w = DC.verbosityMessage w (DC.Decode m w) m
    // RV64ic decodes every constructor except C.JAL (RV32-only)
    vm (cADDI4SPN 8 16); vm (cADDI 1 1); vm (cADDIW 5 1); vm (cLI 5 1); vm (cLUI 5 1)
    vm (cANDI 8 0xF); vm (cLWSP 5 4); vm (cLDSP 5 8)
    vm (cLW 9 8 4); vm (cLD 9 8 8); vm (cSW 8 9 4); vm (cSD 8 9 8)
    vm (cSRLI 8 4); vm (cSRAI 8 2); vm (cSLLI 5 4)
    vm (caEnc 0 0b00 8 9); vm (caEnc 0 0b01 8 9); vm (caEnc 0 0b10 8 9); vm (caEnc 0 0b11 8 9)
    vm (caEnc 1 0b00 8 9); vm (caEnc 1 0b01 8 9)              // C.SUBW C.ADDW
    vm (cJ 16); vm (cADDI16SP 32); vm (cBEQZ 8 16); vm (cBNEZ 8 16)
    vm (crEnc 0 5 0); vm (crEnc 1 5 0)                        // C.JR C.JALR
    vm (crEnc 0 5 6); vm (crEnc 1 5 6)                        // C.MV C.ADD
    vm (cSWSP 5 4); vm (cSDSP 5 8)
    // C.JAL is RV32-only (on RV64 funct3=001 decodes as C.ADDIW)
    let m32 = st RV32ic
    DC.verbosityMessage (cJAL 16) (DC.Decode m32 (cJAL 16)) m32
    DC.verbosityMessage 0 DC.None m         // _ -> "Undef"
    DC.verbosityMessage 0 DC.C_EBREAK m     // _ -> "Undef"
