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
    | RV32ic
    | RV64ic
    | RV32imc
    | RV64imc
    | RV32iac
    | RV64iac
    | RV32imac
    | RV64imac
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
        | "rv32ic"  -> Some(RV32ic)
        | "rv64ic"  -> Some(RV64ic)
        | "rv32imc" -> Some(RV32imc)
        | "rv64imc" -> Some(RV64imc)
        | "rv32iac" -> Some(RV32iac)
        | "rv64iac" -> Some(RV64iac)
        | "rv32imac" -> Some(RV32imac)
        | "rv64imac" -> Some(RV64imac)
        | _ -> None

    member x.archBits = // Get architecture bits
        match x with
        | RV32 | RV32i | RV32im | RV32ia | RV32ima
        | RV32ic | RV32imc | RV32iac | RV32imac -> RV32
        | _ -> RV64

    // Standard-extension predicates (used for decode gating in Decoder.fs)
    member x.hasM =
        match x with
        | RV32im | RV32imc | RV32ima | RV32imac
        | RV64im | RV64imc | RV64ima | RV64imac -> true
        | _ -> false
    member x.hasA =
        match x with
        | RV32ia | RV32iac | RV32ima | RV32imac
        | RV64ia | RV64iac | RV64ima | RV64imac -> true
        | _ -> false
    member x.hasC =
        match x with
        | RV32ic | RV32imc | RV32iac | RV32imac
        | RV64ic | RV64imc | RV64iac | RV64imac -> true
        | _ -> false

type TrapErrors =
    | InstructionFetch of MachineInt
    | InstructionDecode
    | InstructionExecute
    | JumpAddress
    | BreakAddress
    | ECall
    | EBreak
    | MemAddress of MachineInt
    | StepLimit
