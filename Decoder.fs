/// Decode instructions set
module ISA.RISCV.Decoder

open ISA.RISCV.Decode
open ISA.RISCV.Decode.I64
open ISA.RISCV.Arch

type Instructions =
    | I   of I.InstructionI
    | I64 of InstructionI64
    | None

let decode (instr: InstrField) : Instructions =
    let dec = I.DecodeI instr
    match dec with
    | I.InstructionI.None ->
        Instructions.None
    | _ ->
        Instructions.I dec