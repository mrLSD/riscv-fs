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
    let decI32 = I.Decode mstate instr
    let decI64 = I64.Decode instr
    let decM = M.Decode mstate instr
    let decM64 = M64.Decode mstate instr

    // Check is instruction should be executed
    let execI32 =
        match mstate.Arch with
        | RV32i | RV64i | RV32im | RV64im when decI32 <> I.InstructionI.None -> true
        | _ -> false
    let execI64 =
        match mstate.Arch with
        | RV64i | RV64im when decI64 <> I64.InstructionI64.None -> true
        | _ -> false
    let execM32 =
        match mstate.Arch with
        | RV32im | RV64im when decM <> M.InstructionM.None -> true
        | _ -> false
    let execM64 =
        match mstate.Arch with
        | RV64im when decM64 <> M64.InstructionM64.None -> true
        | _ -> false

    // Decoded instruction and execute ISA function
    if execI32 then
        Some(I.Execute decI32)
    else if execI64 then
        Some(I64.Execute decI64)
    else if execM32 then
        Some(M.Execute decM)
    else if execM64 then
        Some(M64.Execute decM64)
    else
        None
