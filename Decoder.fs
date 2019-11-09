/// Decode instructions set
module ISA.RISCV.Decoder

open ISA.RISCV.Decode
open ISA.RISCV.Arch
open ISA.RISCV.MachineState
open ISA.RISCV.Execute

// Execution Function type is currying with partly applied
// concrete function for specific instruction set
type execFunc = MachineState -> MachineState

//type Instructions =
//    | I   of I.InstructionI * execFunc
//    | I64 of I64.InstructionI64 * execFunc
//    | None

// Aggregate decoded data
let Decode (mstate : MachineState) (instr: InstrField) : execFunc option =
    let decI32 = I.Decode mstate instr
    let decI64 = I64.Decode instr
    let decM = M.Decode mstate instr

    // Set decoded instruction and ISA execution function
    if decI32 <> I.InstructionI.None then
        Some(I.Execute decI32)
    else if decI64 <> I64.InstructionI64.None then
        Some(I64.Execute decI64)
    else if decM <> M.InstructionM.None then
        Some(M.Execute decM)
    else
        None
