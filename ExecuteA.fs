module ISA.RISCV.Execute.A

open ISA.RISCV.Arch
open ISA.RISCV.Decode.A
open ISA.RISCV.MachineState
open ISA.RISCV.Utils.Bits

//=================================================
// LR.W - Load-Reserved Word operation
let execLR_W (rd : Register) (rs1 : Register) (mstate : MachineState) =
    mstate.incPC

//=================================================
// SC_W - 
let execSC_W (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    mstate.incPC

// AMOSWAP_W
let execAMOSWAP_W (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    mstate.incPC

//=================================================
// AMOADD_W - AMO Add Word
let execAMOADD_W (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let addr = mstate.getRegister rs1
    let rs2Val = mstate.getRegister rs2
    let nBytes = 4
    
    let memResult = loadWord mstate.Memory addr
    if memResult.IsNone then
        mstate.setRunState (Trap (MemAddress addr))
    else        
        let resMemOp = (int64 memResult.Value) + rs2Val
        let mstate = 
            Array.fold (fun (ms : MachineState) (addr, data) -> ms.setMemoryByte addr data) mstate
                [| for i in 0..(nBytes-1) -> (addr+(int64 i), byte (resMemOp.bitSlice (i*8+7) (i*8) )) |]
        let mstate = mstate.setRegister rd (int64 memResult.Value)
        mstate.incPC

//=================================================
// AMOXOR_W - AMO Xor Word
let execAMOXOR_W (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let addr = mstate.getRegister rs1
    let rs2Val = mstate.getRegister rs2
    let nBytes = 4
    
    let memResult = loadWord mstate.Memory addr
    if memResult.IsNone then
        mstate.setRunState (Trap (MemAddress addr))
    else        
        let resMemOp = (int64 memResult.Value) ^^^ rs2Val
        let mstate = 
            Array.fold (fun (ms : MachineState) (addr, data) -> ms.setMemoryByte addr data) mstate
                [| for i in 0..(nBytes-1) -> (addr+(int64 i), byte (resMemOp.bitSlice (i*8+7) (i*8) )) |]
        let mstate = mstate.setRegister rd (int64 memResult.Value)
        mstate.incPC

//=================================================
// AMOAND_W - AMO And Word
let execAMOAND_W (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    let addr = mstate.getRegister rs1
    let rs2Val = mstate.getRegister rs2
    let nBytes = 4
    
    let memResult = loadWord mstate.Memory addr
    if memResult.IsNone then
        mstate.setRunState (Trap (MemAddress addr))
    else        
        let resMemOp = (int64 memResult.Value) &&& rs2Val
        let mstate = 
            Array.fold (fun (ms : MachineState) (addr, data) -> ms.setMemoryByte addr data) mstate
                [| for i in 0..(nBytes-1) -> (addr+(int64 i), byte (resMemOp.bitSlice (i*8+7) (i*8) )) |]
        let mstate = mstate.setRegister rd (int64 memResult.Value)
        mstate.incPC
        
// AMOOR_W
let execAMOOR_W (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    mstate.incPC

// AMOMIN_W
let execAMOMIN_W (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    mstate.incPC

// AMOMAX_W
let execAMOMAX_W (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    mstate.incPC

// AMOMINU_W
let execAMOMINU_W (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    mstate.incPC

// AMOMAXU_W    
let execAMOMAXU_W (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    mstate.incPC

// Execute A-instructions
let Execute (instr : InstructionA) (mstate : MachineState) =
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
