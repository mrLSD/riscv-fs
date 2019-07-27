module ISA.RISCV.Arch

type MachineInt   = int64
type Register     = int32
type RegisterVal  = MachineInt
type Opcode       = MachineInt
type InstrField   = MachineInt

// Available RISC-V architectures
type Architecture =
    | RV32
    | RV64
    | RV32i
    | RV64i
    static member fromString (x : string) =
        match x with
        | "rv32i" -> Some(RV32i)
        | "rv64i" -> Some(RV64i)
        | _ -> None

    member x.archBits = // Get architecture bits
        match x with
        | Architecture.RV32 | Architecture.RV32i -> RV32
        | Architecture.RV64 | Architecture.RV64i -> RV64

type TrapErrors =
    | InstructionFetch of MachineInt
    | InstructionDecode
    | JumpAddress
    | BreakAddress
    | ECall
    | EBreak
    | MemAddress of MachineInt
