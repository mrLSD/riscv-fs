module ISA.RISCV.Execute.C

open ISA.RISCV.Arch
open ISA.RISCV.Decode.C
open ISA.RISCV.MachineState

module I = ISA.RISCV.Execute.I
module I64 = ISA.RISCV.Execute.I64

// Execute C-instructions: each compressed op delegates to the base instruction
// it expands to. PC advances by InstrLen (= 2 for compressed), so link addresses
// and branch/jump targets are correct through the shared base semantics.
let Execute (instr : InstructionC) (mstate : MachineState) =
    match instr with
    | C_ADDI4SPN i -> I.execADDI i.rd 2 i.imm mstate
    | C_LW i       -> I.execLW i.rd i.rs1 i.imm mstate
    | C_LD i       -> I64.execLD i.rd i.rs1 i.imm mstate
    | C_SW i       -> I.execSW i.rs1 i.rs2 i.imm mstate
    | C_SD i       -> I64.execSD i.rs1 i.rs2 i.imm mstate
    | C_ADDI i     -> I.execADDI i.rd i.rd i.imm mstate
    | C_ADDIW i    -> I64.execADDIW i.rd i.rd i.imm mstate
    | C_LI i       -> I.execADDI i.rd 0 i.imm mstate
    | C_LUI i      -> I.execLUI i.rd i.imm mstate
    | C_ADDI16SP i -> I.execADDI 2 2 i.imm mstate
    | C_SRLI i     -> I.execSRLI i.rd i.rd i.shamt mstate
    | C_SRAI i     -> I.execSRAI i.rd i.rd i.shamt mstate
    | C_ANDI i     -> I.execANDI i.rd i.rd i.imm mstate
    | C_SUB i      -> I.execSUB i.rd i.rd i.rs2 mstate
    | C_XOR i      -> I.execXOR i.rd i.rd i.rs2 mstate
    | C_OR i       -> I.execOR i.rd i.rd i.rs2 mstate
    | C_AND i      -> I.execAND i.rd i.rd i.rs2 mstate
    | C_SUBW i     -> I64.execSUBW i.rd i.rd i.rs2 mstate
    | C_ADDW i     -> I64.execADDW i.rd i.rd i.rs2 mstate
    | C_J i        -> I.execJAL 0 i.imm mstate
    | C_JAL i      -> I.execJAL 1 i.imm mstate
    | C_BEQZ i     -> I.execBEQ i.rs1 0 i.imm mstate
    | C_BNEZ i     -> I.execBNE i.rs1 0 i.imm mstate
    | C_SLLI i     -> I.execSLLI i.rd i.rd i.shamt mstate
    | C_LWSP i     -> I.execLW i.rd 2 i.imm mstate
    | C_LDSP i     -> I64.execLD i.rd 2 i.imm mstate
    | C_JR i       -> I.execJALR 0 i.rs1 0 mstate
    | C_JALR i     -> I.execJALR 1 i.rs1 0 mstate
    | C_MV i       -> I.execADD i.rd 0 i.rs2 mstate
    | C_ADD i      -> I.execADD i.rd i.rd i.rs2 mstate
    | C_EBREAK     -> I.execEBREAK mstate
    | C_SWSP i     -> I.execSW 2 i.rs2 i.imm mstate
    | C_SDSP i     -> I64.execSD 2 i.rs2 i.imm mstate
    | _ -> mstate.setRunState (Trap InstructionExecute)
