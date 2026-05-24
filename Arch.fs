module ISA.RISCV.Arch

// Basic Machine Int representation - include x32
type MachineInt   = int64
// Basic registers: 0-32
type Register     = int32
// Value of Register
type RegisterVal  = MachineInt
type Opcode       = MachineInt
type InstrField   = int32

// Available RISC-V architectures
type Architecture =
    | RV32
    | RV64
    | RV32i
    | RV64i
    | RV32im
    | RV64im
    | RV32ia
    | RV64ia
    | RV32ima
    | RV64ima
    static member fromString (x : string) =
        match x with
        | "rv32i"  -> Some(RV32i)
        | "rv64i"  -> Some(RV64i)
        | "rv32im" -> Some(RV32im)
        | "rv64im" -> Some(RV64im)
        | "rv32ia" -> Some(RV32ia)
        | "rv64ia" -> Some(RV64ia)
        | "rv32ima" -> Some(RV32ima)
        | "rv64ima" -> Some(RV64ima)
        | _ -> None

    member x.archBits = // Get architecture bits
        match x with
        | RV32 | RV32i | RV32im | RV32ia | RV32ima -> RV32
        | RV64 | RV64i | RV64im | RV64ia | RV64ima -> RV64

type TrapErrors =
    | InstructionFetch of MachineInt
    | InstructionDecode
    | InstructionExecute
    | JumpAddress
    | BreakAddress
    | ECall
    | EBreak
    | MemAddress of MachineInt
