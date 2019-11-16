/// Decode instructions set
module ISA.RISCV.Decoder

open ISA.RISCV.Decode
open ISA.RISCV.Arch
open ISA.RISCV.MachineState
open ISA.RISCV.Execute

// Execution Function type is currying with partly applied
// concrete function for specific instruction set
type execFunc = MachineState -> MachineState

// Aggregate decoded data
let Decode (mstate : MachineState) (instr: InstrField) : execFunc option =
    // Decoded instruction and execute ISA function
    if mstate.Arch.archBits = RV32i then
        let decoded = I.Decode mstate instr
        match decoded with
        | I.InstructionI.None -> None
        | _ -> Some(I.Execute decoded)
    else if mstate.Arch.archBits = RV64i then
        let decoded = I64.Decode instr
        match decoded with
        | I64.InstructionI64.None -> None
        | _ -> Some(I64.Execute decoded)
    else if mstate.Arch.archBits = RV32im then
        let decoded = M.Decode mstate instr
        match decoded with
        | M.InstructionM.None -> None
        | _ -> Some(M.Execute decoded)
    else if mstate.Arch.archBits = RV64im then
        let decoded = M.Decode mstate instr
        match decoded with
        | M.InstructionM.None -> None
        | _ -> Some(M64.Execute decoded)
    else
        None
