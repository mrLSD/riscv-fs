module ISA.RISCV.ExecuteI

open ISA.RISCV.Arch
open ISA.RISCV.Decode.I
open ISA.RISCV.MachineState
open ISA.RISCV.Utils.Bits

//=================================================
// LUI - Load Upper immediate
let execLUI (rd : Register) (imm20 : MachineInt) (mstate : MachineState) =
    let mstate = mstate.setRegister rd imm20
    mstate.incPC

//=================================================
// AUIPC - Add Upper immediate PC
let execAUIPC (rd : Register) (imm20 : MachineInt) (mstate : MachineState) =
    let mstate = mstate.setRegister rd (imm20 + mstate.PC)
    mstate.incPC

//=================================================
// JALR - Jump Relative immediately
let execJALR (rd : Register) (rs1 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    let newPC = ((mstate.getRegister rs1) + imm12) &&& (~~~1L)
    if newPC % 4L <> 0L then
        mstate.setRunState (Trap JumpAddress)
    else if newPC = mstate.PC then
        mstate.setRunState Stopped
    else
        let mstate = mstate.setRegister rd (mstate.PC + 4L)
        mstate.setPC newPC

//=================================================
// JAL - Jump immediately
let execJAL (rd : Register) (imm20 : MachineInt) (mstate : MachineState) =
    let newPC = mstate.PC + int64(imm20)
    if newPC % 4L <> 0L then
        mstate.setRunState (Trap JumpAddress)
    else if newPC = mstate.PC then
        mstate.setRunState Stopped
    else
        let mstate = mstate.setRegister rd (mstate.PC + 4L)
        mstate.setPC newPC

// Basic branch flow
let branch (branchCheck : bool) (rs1 : Register) (rs2 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    let newPC = mstate.PC + imm12
    if newPC % 4L <> 0L then
        mstate.setRunState (Trap BreakAddress)
    else if newPC = mstate.PC then
        mstate.setRunState Stopped
    else
        if branchCheck then
            mstate.setPC newPC
        else
            mstate.incPC

//=================================================
// BEQ - Branch if Equal
let execBEQ (rs1 : Register) (rs2 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    branch (rs1 = rs2) rs1 rs2 imm12 mstate

//=================================================
// BNE - Branch if Not Equal
let execBNE (rs1 : Register) (rs2 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    branch (rs1 <> rs2) rs1 rs2 imm12 mstate

//=================================================
// BLT - Branch if Less Then
let execBLT (rs1 : Register) (rs2 : Register) (imm12 : MachineInt) (mstate : MachineState) =
        branch (rs1 < rs2) rs1 rs2 imm12 mstate

//=================================================
// BGE - Branch if Greater or Equal
let execBGE (rs1 : Register) (rs2 : Register) (imm12 : MachineInt) (mstate : MachineState) =
        branch (rs1 >= rs2) rs1 rs2 imm12 mstate

//=================================================
// BLTU - Branch if Less Then (Unsigned)
let execBLTU (rs1 : Register) (rs2 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    branch (uint64 rs1 < uint64 rs2) rs1 rs2 imm12 mstate

//=================================================
// BGEU - Branch If Greater or Equal (Unsigned)
let execBGEU (rs1 : Register) (rs2 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    branch (uint64 rs1 >= uint64 rs2) rs1 rs2 imm12 mstate

//=================================================
// LB - Load Byte from Memory
let execLB (rd : Register) (rs1 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    let addr = (mstate.getRegister rs1) + imm12
    let memResult = loadByte mstate.Memory addr
    if memResult.IsNone then
        mstate.setRunState (Trap (MemAddress addr))
    else
        let mstate = mstate.setRegister rd (int64 memResult.Value)
        mstate.incPC

//=================================================
// LH - Load Half-word (2 bytes)  from Memory
let execLH (rd : Register) (rs1 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    let addr = (mstate.getRegister rs1) + imm12
    let memResult = loadHalfWord mstate.Memory addr
    if memResult.IsNone then
        mstate.setRunState (Trap (MemAddress addr))
    else
        let mstate = mstate.setRegister rd (int64 memResult.Value)
        mstate.incPC

//=================================================
// LW - Load Word (4 bytes) from Memory
let execLW (rd : Register) (rs1 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    let addr = (mstate.getRegister rs1) + imm12
    let memResult = loadWord mstate.Memory addr
    if memResult.IsNone then
        mstate.setRunState (Trap (MemAddress addr))
    else
        let mstate = mstate.setRegister rd (int64 memResult.Value)
        mstate.incPC

//=================================================
// LBU - Load Byte Unsigned from Memory
let execLBU (rd : Register) (rs1 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    let addr = (mstate.getRegister rs1) + imm12
    let memResult = loadByte mstate.Memory addr
    if memResult.IsNone then
        mstate.setRunState (Trap (MemAddress addr))
    else
        let memVal = uint8 memResult.Value
        let mstate = mstate.setRegister rd (int64 memVal)
        mstate.incPC

//=================================================
// LHU - Load Half-word (2 bytes) Unsigned from Memory
let execLHU (rd : Register) (rs1 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    let addr = (mstate.getRegister rs1) + imm12
    let memResult = loadHalfWord mstate.Memory addr
    if memResult.IsNone then
        mstate.setRunState (Trap (MemAddress addr))
    else
        let memVal = uint16 memResult.Value
        let mstate = mstate.setRegister rd (int64 memVal)
        mstate.incPC

//=================================================
// SB - Store Byte to Memory
let execSB (rs1 : Register) (rs2 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    let addr = (mstate.getRegister rs1) + imm12
    let nBytes = 1
    let rs2Val = mstate.getRegister rs2
    Array.fold (fun (ms : MachineState) (addr, data) -> ms.setMemoryByte addr data) mstate
        [| for i in 0..(nBytes-1) -> (addr+(int64 i), byte (rs2Val.bitSlice (i*8+7) (i*8) )) |]

//=================================================
// SH - Store 2 Bytes (Hald word) to Memory
let execSH (rs1 : Register) (rs2 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    let addr = (mstate.getRegister rs1) + imm12
    let nBytes = 2
    let rs2Val = mstate.getRegister rs2
    Array.fold (fun (ms : MachineState) (addr, data) -> ms.setMemoryByte addr data) mstate
        [| for i in 0..(nBytes-1) -> (addr+(int64 i), byte (rs2Val.bitSlice (i*8+7) (i*8) )) |]

//=================================================
// SW - Store 4 Bytes (Word) to Memory
let execSW (rs1 : Register) (rs2 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    let addr = (mstate.getRegister rs1) + imm12
    let nBytes = 4
    let rs2Val = mstate.getRegister rs2
    Array.fold (fun (ms : MachineState) (addr, data) -> ms.setMemoryByte addr data) mstate
        [| for i in 0..(nBytes-1) -> (addr+(int64 i), byte (rs2Val.bitSlice (i*8+7) (i*8) )) |]

//=================================================
// ADDI - Add immediate
let execADDI (rd : Register) (rs1 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    let rdVal = (mstate.getRegister rs1) + imm12
    let mstate = mstate.setRegister rd rdVal
    mstate.incPC

//=================================================
// SLTI - Set to 1 if Less Then Immediate
let execSLTI (rd : Register) (rs1 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    let rdVal = if (mstate.getRegister rs1) < imm12 then 1L else 0L
    let mstate = mstate.setRegister rd rdVal
    mstate.incPC

//=================================================
// SLTIU - Set to 1 if Less Then Unsign Immediate
let execSLTIU (rd : Register) (rs1 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    let rdVal = if uint64(mstate.getRegister rs1) < uint64 imm12 then 1L else 0L
    let mstate = mstate.setRegister rd rdVal
    mstate.incPC

//=================================================
// XORI - Xor immediately
let execXORI (rd : Register) (rs1 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    let rdVal = (mstate.getRegister rs1) ^^^ imm12
    let mstate = mstate.setRegister rd rdVal
    mstate.incPC

//=================================================
// ORI - Or immediately
let execORI (rd : Register) (rs1 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    let rdVal = (mstate.getRegister rs1) ||| imm12
    let mstate = mstate.setRegister rd rdVal
    mstate.incPC

//=================================================
// SLLI - Shift Left Logical Immediate
let execSLLI (rd : Register) (rs1 : Register) (shamt : MachineInt) (mstate : MachineState) =
    let rdVal = (mstate.getRegister rs1) <<< int32 shamt
    let mstate = mstate.setRegister rd rdVal
    mstate.incPC

//=================================================
// SRLI - Shift Right Logical Immediate
let execSRLI (rd : Register) (rs1 : Register) (shamt : MachineInt) (mstate : MachineState) =
    let rdVal = int64(uint64(mstate.getRegister rs1) >>> int32 shamt)
    let mstate = mstate.setRegister rd rdVal
    mstate.incPC

//=================================================
// SRAI - Shift Right Arithmetic Immediate
let execSRAI (rd : Register) (rs1 : Register) (shamt : MachineInt) (mstate : MachineState) =
    let rdVal = (mstate.getRegister rs1) >>> int32 shamt
    let mstate = mstate.setRegister rd rdVal
    mstate.incPC

//=================================================
// ANDI - And immediately
let execANDI (rd : Register) (rs1 : Register) (imm12 : MachineInt) (mstate : MachineState) =
    let rdVal = (mstate.getRegister rs1) &&& imm12
    let mstate = mstate.setRegister rd rdVal
    mstate.incPC

//=================================================
// ADD - Add operation
let execADD (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let rdVal = (mstate.getRegister rs1) + (mstate.getRegister rs2)
    let mstate = mstate.setRegister rd rdVal
    mstate.incPC

//=================================================
// SUB - Sub operation
let execSUB (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let rdVal = (mstate.getRegister rs1) - (mstate.getRegister rs2)
    let mstate = mstate.setRegister rd rdVal
    mstate.incPC

//=================================================
// SLL - Shift Logical Left
let execSLL (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let rdVal = (mstate.getRegister rs1) <<< int32(mstate.getRegister rs2)
    let mstate = mstate.setRegister rd rdVal
    mstate.incPC

//=================================================
// SLT - Set 1 if Less Then
let execSLT (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let rdVal = if mstate.getRegister rs1 < mstate.getRegister rs2 then 1L else 0L
    let mstate = mstate.setRegister rd rdVal
    mstate.incPC

//=================================================
// SLTU - Set to 1 if Less Then Unsign Immediate
let execSLTU (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let rdVal = if uint64(mstate.getRegister rs1) < uint64(mstate.getRegister rs2) then 1L else 0L
    let mstate = mstate.setRegister rd rdVal
    mstate.incPC

//=================================================
// XOR - Xor operation
let execXOR (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let rdVal = (mstate.getRegister rs1) ^^^ (mstate.getRegister rs2)
    let mstate = mstate.setRegister rd rdVal
    mstate.incPC

//=================================================
// SRL - Shift Right Logical
let execSRL (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let rdVal = int64(uint64(mstate.getRegister rs1) >>> int32(mstate.getRegister rs2))
    let mstate = mstate.setRegister rd rdVal
    mstate.incPC

//=================================================
// SRA - Shift Right Arithmetic
let execSRA (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let rdVal = (mstate.getRegister rs1) >>> int32 (mstate.getRegister rs2)
    let mstate = mstate.setRegister rd rdVal
    mstate.incPC

//=================================================
// OR - Or operation
let execOR (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let rdVal = (mstate.getRegister rs1) ||| (mstate.getRegister rs2)
    let mstate = mstate.setRegister rd rdVal
    mstate.incPC

//=================================================
// AND - And operation
let execAND (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let rdVal = (mstate.getRegister rs1) &&& (mstate.getRegister rs2)
    let mstate = mstate.setRegister rd rdVal
    mstate.incPC

//=================================================
// Fence - Fence operation (no operations)
let execFENCE (mstate : MachineState) =
    mstate.incPC

//=================================================
// ECALL - ECALL operation
let execECALL (mstate : MachineState) =
    mstate.setRunState (Trap ECall)

//=================================================
// execEBREAK - EBREAK operation
let execEBREAK (mstate : MachineState) =
    mstate.setRunState (Trap EBreak)

// Execute I-instructions
let ExecuteI (instr : InstructionI) (mstate : MachineState) =
    match instr with
    | LUI i ->
        execLUI i.rd i.imm20 mstate
    | AUIPC i ->
        execAUIPC i.rd i.imm20 mstate
    | JALR i ->
        execJALR i.rd i.rs1 i.imm12 mstate
    | JAL i ->
        execJAL i.rd i.imm20 mstate
    | BEQ i ->
        execBEQ i.rs1 i.rs2 i.imm12 mstate
    | BNE i ->
        execBNE i.rs1 i.rs2 i.imm12 mstate
    | BLT i ->
        execBLT i.rs1 i.rs2 i.imm12 mstate
    | BGE i ->
        execBGE i.rs1 i.rs2 i.imm12 mstate
    | BLTU i ->
        execBLTU i.rs1 i.rs2 i.imm12 mstate
    | BGEU i ->
        execBGEU i.rs1 i.rs2 i.imm12 mstate
    | LB i ->
        execLB i.rd i.rs1 i.imm12 mstate
    | LH i ->
        execLH i.rd i.rs1 i.imm12 mstate
    | LW i ->
        execLW i.rd i.rs1 i.imm12 mstate
    | LBU i ->
        execLBU i.rd i.rs1 i.imm12 mstate
    | LHU i ->
        execLHU i.rd i.rs1 i.imm12 mstate
    | SB i ->
        execSB i.rs1 i.rs2 i.imm12 mstate
    | SH i ->
        execSH i.rs1 i.rs2 i.imm12 mstate
    | SW i ->
        execSW i.rs1 i.rs2 i.imm12 mstate
    | ADDI i ->
        execADDI i.rd i.rs1 i.imm12 mstate
    | SLTI i ->
        execSLTI i.rd i.rs1 i.imm12 mstate
    | SLTIU i ->
        execSLTIU i.rd i.rs1 i.imm12 mstate
    | XORI i ->
        execXORI i.rd i.rs1 i.imm12 mstate
    | ORI i ->
        execORI i.rd i.rs1 i.imm12 mstate
    | ANDI i ->
        execANDI i.rd i.rs1 i.imm12 mstate
    | SLLI i ->
        execSLLI i.rd i.rs1 i.shamt mstate
    | SRLI i ->
        execSRLI i.rd i.rs1 i.shamt mstate
    | SRAI i ->
        execSRAI i.rd i.rs1 i.shamt mstate
    | ADD i ->
        execADD i.rd i.rs1 i.rs2 mstate
    | SUB i ->
        execSUB i.rd i.rs1 i.rs2 mstate
    | SLL i ->
        execSLL i.rd i.rs1 i.rs2 mstate
    | SLT i ->
        execSLT i.rd i.rs1 i.rs2 mstate
    | SLTU i ->
        execSLTU i.rd i.rs1 i.rs2 mstate
    | XOR i ->
        execXOR i.rd i.rs1 i.rs2 mstate
    | SRL i ->
        execSRL i.rd i.rs1 i.rs2 mstate
    | SRA i ->
        execSRA i.rd i.rs1 i.rs2 mstate
    | OR i ->
        execOR i.rd i.rs1 i.rs2 mstate
    | AND i ->
        execAND i.rd i.rs1 i.rs2 mstate
    | FENCE _ ->
        execFENCE mstate
    | ECALL _ ->
        execECALL mstate
    | EBREAK _ ->
        execEBREAK mstate
    | _ -> mstate
