/// Decode instructions set

module ISA.RISCV.Decoder

open ISA.RISCV.Decode.I

type Instructions =
    | I of InstructionI
