module ISA.RISCV.Execute.I64

open ISA.RISCV.Arch
open ISA.RISCV.Decode.I64
open ISA.RISCV.MachineState
open ISA.RISCV.Utils.Bits

//=================================================
// LWU - Load Word (4 bytes) Unsigned from Memory
let execLWU (rd : Register) (rs1 : Register) (imm12 : InstrField) (mstate : MachineState) =
    let addr = (mstate.getRegister rs1) + int64 imm12
    let memResult = loadHalfWord mstate.Memory addr
    if memResult.IsNone then
        mstate.setRunState (Trap (MemAddress addr))
    else
        let memVal = uint16 memResult.Value
        let mstate = mstate.setRegister rd (int64 (uint16 memVal))
        mstate.incPC

//=================================================
// LD - Load double Word (8 bytes) from Memory
let execLD (rd : Register) (rs1 : Register) (imm12 : InstrField) (mstate : MachineState) =
    let addr = (mstate.getRegister rs1) + int64 imm12
    let memResult = loadDouble mstate.Memory addr
    if memResult.IsNone then
        mstate.setRunState (Trap (MemAddress addr))
    else
        let memVal = uint16 memResult.Value
        let mstate = mstate.setRegister rd (int64 (uint16 memVal))
        mstate.incPC

//=================================================
// SD - Store double Word (8 bytes) to Memory
let execSD (rd : Register) (rs1 : Register) (imm12 : InstrField) (mstate : MachineState) =
    let addr = (mstate.getRegister rs1) + int64 imm12
    let memResult = loadHalfWord mstate.Memory addr
    if memResult.IsNone then
        mstate.setRunState (Trap (MemAddress addr))
    else
        let memVal = uint16 memResult.Value
        let mstate = mstate.setRegister rd (int64 (uint16 memVal))
        mstate.incPC

// Execute I64-instructions
let Execute (instr : InstructionI64) (mstate : MachineState) =
    match instr with
    | LWU i ->
        execLWU i.rd i.rs1 i.imm12 mstate
    | LD i ->
        execLD i.rd i.rs1 i.imm12 mstate
    | SD i ->
        execSD i.rd i.rs1 i.imm12 mstate
    | _ -> mstate.setRunState (Trap InstructionExecute)
