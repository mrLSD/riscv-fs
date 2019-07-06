module ISA.RISCV.Arch

type MachineInt   = int32
type Register     = int32
type Opcode       = MachineInt
type InstrField   = MachineInt

type Architecture =
    | RV32
    | RV64

type TrapErrors =
    | InstructionFetch
    | InstructionDecode
