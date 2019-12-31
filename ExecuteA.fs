module ISA.RISCV.Execute.A

open ISA.RISCV.Arch
open ISA.RISCV.Decode.A
open ISA.RISCV.MachineState

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

// AMOADD_W
let execAMOADD_W (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    mstate.incPC

// AMOXOR_W
let execAMOXOR_W (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    mstate.incPC

// AMOAND_W
let execAMOAND_W (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
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
