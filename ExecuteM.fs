module ISA.RISCV.Execute.M

open ISA.RISCV.Arch
open ISA.RISCV.Decode
open ISA.RISCV.MachineState
open ISA.RISCV.Utils.Bits

// Execute M-instructions
let Execute (instr : M.InstructionM) (mstate : MachineState) =
    match instr with
    | _ -> mstate.setRunState (Trap InstructionExecute)
// mul  = s*s
// mulw = 32(s)*32(s) -> sign_ext
// mulh = s*s -> [63..32]
// mulhsu = s*us(32|64) -> [63..32]
// mulhu = us(32|64)*us(32|64) -> [63..32]
