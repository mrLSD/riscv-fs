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
    let decA = A.Decode instr
    let decA64 = A64.Decode instr
    let decC = C.Decode mstate instr

    let rv64 = mstate.Arch.archBits = RV64

    // Each extension is gated by architecture support; base I is always present.
    // (Compressed and 32-bit encodings are disjoint by inst[1:0], so only one decodes.)
    let execI32 = decI32 <> I.InstructionI.None
    let execI64 = rv64 && decI64 <> I64.InstructionI64.None
    let execM32 = mstate.Arch.hasM && decM <> M.InstructionM.None
    let execM64 = rv64 && mstate.Arch.hasM && decM64 <> M64.InstructionM64.None
    let execA32 = mstate.Arch.hasA && decA <> A.InstructionA.None
    let execA64 = rv64 && mstate.Arch.hasA && decA64 <> A64.InstructionA64.None
    let execC   = mstate.Arch.hasC && decC <> C.InstructionC.None

    // Instruction length is intrinsic to the encoding (inst[1:0]=11 => 32-bit, else
    // 16-bit compressed). Bake it into the returned executor so PC/link advance
    // correctly for any caller, without relying on InstrLen being set externally.
    let len = if (instr &&& 0x3) = 0x3 then 4 else 2

    // Decoded instruction and execute ISA function
    let executor =
        if execI32 then
            Some(I.Execute decI32)
        else if execI64 then
            Some(I64.Execute decI64)
        else if execM32 then
            Some(M.Execute decM)
        else if execM64 then
            Some(M64.Execute decM64)
        else if execA32 then
            Some(A.Execute decA)
        else if execA64 then
            Some(A64.Execute decA64)
        else if execC then
            Some(C.Execute decC)
        else
            None
    executor |> Option.map (fun exec ms -> exec { ms with InstrLen = len })
