module ISA.RISCV.Execute.A64

open ISA.RISCV.Arch
open ISA.RISCV.Decode.A64
open ISA.RISCV.MachineState

//=================================================
// LR.D - Load-Reserved Double Word operation
let execLR_D (rd : Register) (rs1 : Register) (mstate : MachineState) =
    mstate.incPC

// SC_D
let execSC_D (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    mstate.incPC

// AMOSWAP_D
let execAMOSWAP_D (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    mstate.incPC

// AMOADD_D    
let execAMOADD_D (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
    mstate.incPC

// AMOXOR_D
let execAMOXOR_D (rd : Register) (rs1 : Register) (rs2 : Register) (mstate : MachineState) =
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
