module ISA.RISCV.Arch

type MachineInt   = int64
type Register     = int64
type Opcode       = MachineInt
type InstrField   = MachineInt

type Architecture =
    | RV32
    | RV64

type TrapErrors =
    | InstructionFetch
    | InstructionDecode
    | JumpAddress
