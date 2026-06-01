module ISA.RISCV.Execute.A

open ISA.RISCV.Arch
open ISA.RISCV.Decode.A
open ISA.RISCV.MachineState
open ISA.RISCV.Utils.Bits

//=================================================
// LR.W - Load-Reserved Word operation
// This acts just like a lw in this implementation (no need for sync)
// (except there's no immediate)
let execLR_W (rd : Register) (rs1 : Register) (mstate : MachineState) =
    let addr = mstate.getRegister rs1
    let memResult = loadWord mstate.Memory addr
    if memResult.IsNone then
        mstate.setRunState (Trap (MemAddress addr))
    else
        let mstate = mstate.setRegister rd (int64 memResult.Value)
        let mstate = mstate.setReservation addr
        mstate.incPC

//=================================================
// SC.W - Store Conditional Word
// This acts just like a sd in this implementation, but it will
// always set the check register to 0 (indicating load success)
let execSC_W (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let addr = mstate.getRegister rs1
    if mstate.Reservation = Some addr then
        let mstate = mstate.storeMemoryWord addr (mstate.getRegister rs2)
        let mstate = mstate.setRegister rd 0L
        let mstate = mstate.clearReservation
        mstate.incPC
    else
        let mstate = mstate.setRegister rd 1L
        let mstate = mstate.clearReservation
        mstate.incPC

//=================================================
// AMOSWAP.W - AMO Swap word
let execAMOSWAP_W (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let addr = mstate.getRegister rs1
    let rs2Val = mstate.getRegister rs2
    
    let memResult = loadWord mstate.Memory addr
    if memResult.IsNone then
        mstate.setRunState (Trap (MemAddress addr))
    else        
        let resMemOp = rs2Val
        let mstate = mstate.storeMemoryWord addr resMemOp
        let mstate = mstate.setRegister rd (int64 memResult.Value)
        mstate.incPC

//=================================================
// AMOADD.W - AMO Add Word
let execAMOADD_W (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let addr = mstate.getRegister rs1
    let rs2Val = mstate.getRegister rs2
    
    let memResult = loadWord mstate.Memory addr
    if memResult.IsNone then
        mstate.setRunState (Trap (MemAddress addr))
    else
        let resMemOp = (int64 memResult.Value) + rs2Val
        let mstate = mstate.storeMemoryWord addr resMemOp
        let mstate = mstate.setRegister rd (int64 memResult.Value)
        mstate.incPC

//=================================================
// AMOXOR.W - AMO Xor Word
let execAMOXOR_W (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let addr = mstate.getRegister rs1
    let rs2Val = mstate.getRegister rs2
    
    let memResult = loadWord mstate.Memory addr
    if memResult.IsNone then
        mstate.setRunState (Trap (MemAddress addr))
    else        
        let resMemOp = (int64 memResult.Value) ^^^ rs2Val
        let mstate = mstate.storeMemoryWord addr resMemOp
        let mstate = mstate.setRegister rd (int64 memResult.Value)
        mstate.incPC

//=================================================
// AMOAND.W - AMO And Word
let execAMOAND_W (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let addr = mstate.getRegister rs1
    let rs2Val = mstate.getRegister rs2
    
    let memResult = loadWord mstate.Memory addr
    if memResult.IsNone then
        mstate.setRunState (Trap (MemAddress addr))
    else        
        let resMemOp = (int64 memResult.Value) &&& rs2Val
        let mstate = mstate.storeMemoryWord addr resMemOp
        let mstate = mstate.setRegister rd (int64 memResult.Value)
        mstate.incPC

//=================================================
// AMOOR.W - AMO Or Word
let execAMOOR_W (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let addr = mstate.getRegister rs1
    let rs2Val = mstate.getRegister rs2
    
    let memResult = loadWord mstate.Memory addr
    if memResult.IsNone then
        mstate.setRunState (Trap (MemAddress addr))
    else        
        let resMemOp = (int64 memResult.Value) ||| rs2Val
        let mstate = mstate.storeMemoryWord addr resMemOp
        let mstate = mstate.setRegister rd (int64 memResult.Value)
        mstate.incPC

//=================================================
// AMOMIN.W - AMO Min Word
let execAMOMIN_W (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let addr = mstate.getRegister rs1
    let rs2Val = mstate.getRegister rs2
    
    let memResult = loadWord mstate.Memory addr
    if memResult.IsNone then
        mstate.setRunState (Trap (MemAddress addr))
    else        
        let resMemOp =
            if memResult.Value > int32 rs2Val then
                rs2Val
            else
                int64 memResult.Value
        let mstate = mstate.storeMemoryWord addr resMemOp
        let mstate = mstate.setRegister rd (int64 memResult.Value)
        mstate.incPC

//=================================================
// AMOMAX.W - AMO Max Word
let execAMOMAX_W (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let addr = mstate.getRegister rs1
    let rs2Val = mstate.getRegister rs2
    
    let memResult = loadWord mstate.Memory addr
    if memResult.IsNone then
        mstate.setRunState (Trap (MemAddress addr))
    else        
        let resMemOp =
            if memResult.Value < int32 rs2Val then
                rs2Val
            else
                int64 memResult.Value
        let mstate = mstate.storeMemoryWord addr resMemOp
        let mstate = mstate.setRegister rd (int64 memResult.Value)
        mstate.incPC

//=================================================
// AMOMINU.W - AMO Unsigned Min Word
let execAMOMINU_W (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let addr = mstate.getRegister rs1
    let rs2Val = mstate.getRegister rs2
    
    let memResult = loadWord mstate.Memory addr
    if memResult.IsNone then
        mstate.setRunState (Trap (MemAddress addr))
    else        
        let resMemOp =
            if (uint32 memResult.Value) > (uint32 rs2Val) then
                rs2Val
            else
                int64 memResult.Value
        let mstate = mstate.storeMemoryWord addr resMemOp
        let mstate = mstate.setRegister rd (int64 memResult.Value)
        mstate.incPC

//=================================================
// AMOMAXU.W - AMO Unsigned Max Word    
let execAMOMAXU_W (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let addr = mstate.getRegister rs1
    let rs2Val = mstate.getRegister rs2
    
    let memResult = loadWord mstate.Memory addr
    if memResult.IsNone then
        mstate.setRunState (Trap (MemAddress addr))
    else        
        let resMemOp =
            if (uint32 memResult.Value) < (uint32 rs2Val) then
                rs2Val
            else
                int64 memResult.Value
        let mstate = mstate.storeMemoryWord addr resMemOp
        let mstate = mstate.setRegister rd (int64 memResult.Value)
        mstate.incPC

// Execute A-instructions
let Execute (instr : InstructionA) (mstate : MachineState) =
    let addr =
        match instr with
        | LR_W i -> mstate.getRegister i.rs1
        | SC_W i | AMOSWAP_W i | AMOADD_W i | AMOXOR_W i | AMOAND_W i | AMOOR_W i
        | AMOMIN_W i | AMOMAX_W i | AMOMINU_W i | AMOMAXU_W i -> mstate.getRegister i.rs1
        | InstructionA.None -> 0L
    if instr <> InstructionA.None && addr % 4L <> 0L then
        mstate.setRunState (Trap (MemAddress addr))
    else
    match instr with
    | LR_W i ->
        execLR_W i.rd i.rs1 mstate
    | SC_W i ->
        execSC_W i.rd i.rs1 i.rs2 mstate
    | AMOSWAP_W i ->
        execAMOSWAP_W i.rd i.rs1 i.rs2 mstate
    | AMOADD_W i ->
        execAMOADD_W i.rd i.rs1 i.rs2 mstate
    | AMOXOR_W i ->
        execAMOXOR_W i.rd i.rs1 i.rs2 mstate
    | AMOAND_W i ->
        execAMOAND_W i.rd i.rs1 i.rs2 mstate
    | AMOOR_W i ->
        execAMOOR_W i.rd i.rs1 i.rs2 mstate
    | AMOMIN_W i ->
        execAMOMIN_W i.rd i.rs1 i.rs2 mstate
    | AMOMAX_W i ->
        execAMOMAX_W i.rd i.rs1 i.rs2 mstate
    | AMOMINU_W i ->
        execAMOMINU_W i.rd i.rs1 i.rs2 mstate
    | AMOMAXU_W i ->
        execAMOMAXU_W i.rd i.rs1 i.rs2 mstate
                
    | _ -> mstate.setRunState (Trap InstructionExecute)
