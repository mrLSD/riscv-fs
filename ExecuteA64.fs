module ISA.RISCV.Execute.A64

open ISA.RISCV.Arch
open ISA.RISCV.Decode.A64
open ISA.RISCV.MachineState
open ISA.RISCV.Utils.Bits

//=================================================
// LR.D - Load-Reserved Double Word operation
// This acts just like a lw in this implementation (no need for sync)
// (except there's no immediate)
let execLR_D (rd : Register) (rs1 : Register) (mstate : MachineState) =
    let addr = mstate.getRegister rs1
    let memResult = loadDouble mstate.Memory addr
    if memResult.IsNone then
        mstate.setRunState (Trap (MemAddress addr))
    else
        let mstate = mstate.setRegister rd (int64 memResult.Value)
        mstate.incPC

//=================================================
// SC.D - Store Conditional Double Word
// This acts just like a sd in this implementation, but it will
// always set the check register to 0 (indicating load success)
let execSC_D (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let addr = mstate.getRegister rs1
    let resMemOp = mstate.getRegister rs2
    let mstate = mstate.storeMemoryDoubleWord addr resMemOp
    let mstate = mstate.setRegister rd 0L
    mstate.incPC

//=================================================
// AMOSWAP.D - AMO Swap Double word
let execAMOSWAP_D (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let addr = mstate.getRegister rs1
    let rs2Val = mstate.getRegister rs2
    
    let memResult = loadDouble mstate.Memory addr
    if memResult.IsNone then
        mstate.setRunState (Trap (MemAddress addr))
    else        
        let resMemOp = rs2Val
        let mstate = mstate.storeMemoryDoubleWord addr resMemOp
        let mstate = mstate.setRegister rd (int64 memResult.Value)
        mstate.incPC

//=================================================
// AMOADD.D - AMO Add Double Word    
let execAMOADD_D (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let addr = mstate.getRegister rs1
    let rs2Val = mstate.getRegister rs2
    
    let memResult = loadDouble mstate.Memory addr
    if memResult.IsNone then
        mstate.setRunState (Trap (MemAddress addr))
    else        
        let resMemOp = (int64 memResult.Value) + rs2Val
        let mstate = mstate.storeMemoryDoubleWord addr resMemOp
        let mstate = mstate.setRegister rd (int64 memResult.Value)
        mstate.incPC

//=================================================
// AMOXOR.W - AMO Xor Double Word
let execAMOXOR_D (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let addr = mstate.getRegister rs1
    let rs2Val = mstate.getRegister rs2
    
    let memResult = loadDouble mstate.Memory addr
    if memResult.IsNone then
        mstate.setRunState (Trap (MemAddress addr))
    else        
        let resMemOp = (int64 memResult.Value) ^^^ rs2Val
        let mstate = mstate.storeMemoryDoubleWord addr resMemOp
        let mstate = mstate.setRegister rd (int64 memResult.Value)
        mstate.incPC

// AMOAND_D
let execAMOAND_D (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    mstate.incPC

// AMOOR_D
let execAMOOR_D (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    mstate.incPC

// AMOMIN_D
let execAMOMIN_D (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    mstate.incPC

// AMOMAX_D
let execAMOMAX_D (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    mstate.incPC

// AMOMINU_D
let execAMOMINU_D (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    mstate.incPC

// AMOMAXU_D
let execAMOMAXU_D (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    mstate.incPC

// Execute A-instructions
let Execute (instr : InstructionA64) (mstate : MachineState) =
    match instr with
    | LR_D i ->
        execLR_D i.rd i.rs1 mstate
    | SC_D i ->
        execSC_D i.rd i.rs1 i.rs2 mstate        
    | AMOSWAP_D i ->
        execAMOSWAP_D i.rd i.rs1 i.rs2 mstate
    | AMOADD_D i ->
        execAMOADD_D i.rd i.rs1 i.rs2 mstate
    | AMOXOR_D i ->
        execAMOXOR_D i.rd i.rs1 i.rs2 mstate
    | AMOAND_D i ->
        execAMOAND_D i.rd i.rs1 i.rs2 mstate
    | AMOOR_D i ->
        execAMOOR_D i.rd i.rs1 i.rs2 mstate
    | AMOMIN_D i ->
        execAMOMIN_D i.rd i.rs1 i.rs2 mstate
    | AMOMAX_D i ->
        execAMOMAX_D i.rd i.rs1 i.rs2 mstate
    | AMOMINU_D i ->
        execAMOMINU_D i.rd i.rs1 i.rs2 mstate
    | AMOMAXU_D i ->
        execAMOMAXU_D i.rd i.rs1 i.rs2 mstate
                
    | _ -> mstate.setRunState (Trap InstructionExecute)
