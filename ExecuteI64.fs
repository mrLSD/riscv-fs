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
        let memVal = uint64 memResult.Value
        let mstate = mstate.setRegister rd (int64 memVal)
        mstate.incPC

//=================================================
// LD - Load double Word (8 bytes) from Memory
let execLD (rd : Register) (rs1 : Register) (imm12 : InstrField) (mstate : MachineState) =
    let addr = (mstate.getRegister rs1) + int64 imm12
    let memResult = loadDouble mstate.Memory addr
    if memResult.IsNone then
        mstate.setRunState (Trap (MemAddress addr))
    else
        let mstate = mstate.setRegister rd (int64 memResult.Value)
        mstate.incPC

//=================================================
// SD - Store double Word (8 bytes) to Memory
let execSD (rs1 : Register) (rs2 : Register) (imm12 : InstrField) (mstate : MachineState) =
    let addr = (mstate.getRegister rs1) + int64 imm12
    let nBytes = 8
    let rs2Val = mstate.getRegister rs2
    Array.fold (fun (ms : MachineState) (addr, data) -> ms.setMemoryByte addr data) mstate
        [| for i in 0..(nBytes-1) -> (addr+(int64 i), byte (rs2Val.bitSlice (i*8+7) (i*8) )) |]

//=================================================
// ADDIW - Add immediate Word
// Returns sign-extension to 64 bits of lower 32 bits of result.
let execADDIW (rd : Register) (rs1 : Register) (imm12 : InstrField) (mstate : MachineState) =
    let rdVal = int32(mstate.getRegister rs1) + int32 imm12
    let mstate = mstate.setRegister rd (int64 rdVal)
    mstate.incPC

//=================================================
// SLLIW - Shift Left Logical Immediate Word
// Returns sign-extension to 64 bits of lower 32 bits of result.
let execSLLIW (rd : Register) (rs1 : Register) (shamt : InstrField) (mstate : MachineState) =
    let rdVal = int32(mstate.getRegister rs1) <<< int32 shamt
    let mstate = mstate.setRegister rd (int64 rdVal)
    mstate.incPC

//=================================================
// SRLIW - Shift Right Logical Immediate Word
// Returns sign-extension to 64 bits of lower 32 bits of result.
let execSRLIW (rd : Register) (rs1 : Register) (shamt : InstrField) (mstate : MachineState) =
    let rdVal = int32(uint32(mstate.getRegister rs1) >>> int32 shamt)
    let mstate = mstate.setRegister rd (int64 rdVal)
    mstate.incPC

//=================================================
// SRAIW - Shift Right Arithmetic Immediate Word
// Returns sign-extension to 64 bits of lower 32 bits of result.
let execSRAIW (rd : Register) (rs1 : Register) (shamt : InstrField) (mstate : MachineState) =
    let rdVal = int32(mstate.getRegister rs1) >>> int32 shamt
    let mstate = mstate.setRegister rd (int64 rdVal)
    mstate.incPC

//=================================================
// ADDW - Add operation Word
// Returns sign-extension to 64 bits of lower 32 bits of result.
let execADDW (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let rdVal = int32(mstate.getRegister rs1) + int32(mstate.getRegister rs2)
    let mstate = mstate.setRegister rd (int64 rdVal)
    mstate.incPC

//=================================================
// SUBW - Sub operation Word
// Returns sign-extension to 64 bits of lower 32 bits of result.
let execSUBW (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let rdVal = int32(mstate.getRegister rs1) - int32(mstate.getRegister rs2)
    let mstate = mstate.setRegister rd (int64 rdVal)
    mstate.incPC

//=================================================
// SLLW - Shift Logical Left Word
// Returns sign-extension to 64 bits of lower 32 bits of result.
let execSLLW (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let rdVal = int32(mstate.getRegister rs1) <<< int32(mstate.getRegister rs2)
    let mstate = mstate.setRegister rd (int64 rdVal)
    mstate.incPC

//=================================================
// SRLW - Shift Right Logical Word
// Returns sign-extension to 64 bits of lower 32 bits of result.
let execSRLW (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let rdVal = int32(uint32(mstate.getRegister rs1) >>> int32(mstate.getRegister rs2))
    let mstate = mstate.setRegister rd (int64 rdVal)
    mstate.incPC

//=================================================
// SRAW - Shift Right Arithmetic Word
// Returns sign-extension to 64 bits of lower 32 bits of result.
let execSRAW (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let rdVal = int32(mstate.getRegister rs1) >>> int32 (mstate.getRegister rs2)
    let mstate = mstate.setRegister rd (int64 rdVal)
    mstate.incPC

// Execute I64-instructions
let Execute (instr : InstructionI64) (mstate : MachineState) =
    match instr with
    | LWU i ->
        execLWU i.rd i.rs1 i.imm12 mstate
    | LD i ->
        execLD i.rd i.rs1 i.imm12 mstate
    | SD i ->
        execSD i.rs1 i.rs2 i.imm12 mstate
    | ADDIW i ->
        execADDIW i.rd i.rs1 i.imm12 mstate
    | SLLIW i ->
        execSLLIW i.rd i.rs1 i.shamt mstate
    | SRLIW i ->
        execSRLIW i.rd i.rs1 i.shamt mstate
    | SRAIW i ->
        execSRAIW i.rd i.rs1 i.shamt mstate
    | ADDW i ->
        execADDW i.rd i.rs1 i.rs2 mstate
    | SUBW i ->
        execSUBW i.rd i.rs1 i.rs2 mstate
    | SLLW i ->
        execSLLW i.rd i.rs1 i.rs2 mstate
    | SRLW i ->
        execSRLW i.rd i.rs1 i.rs2 mstate
    | SRAW i ->
        execSRAW i.rd i.rs1 i.rs2 mstate
    | _ -> mstate.setRunState (Trap InstructionExecute)
