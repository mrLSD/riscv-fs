module ISA.RISCV.MachineState

open System
open Microsoft.FSharp.Collections
open ISA.RISCV.Arch

type RunMachineState =
    | NotRun
    | Run
    | Stopped
    | Trap of TrapErrors

type MachineState = {
        PC:         MachineInt
        Registers:  RegisterVal array
        Memory:     Map<int64, byte>
        Verbosity:  bool
        RunState:   RunMachineState
    } with
    member x.getRegister(reg: Register) : MachineInt =
        x.Registers.[reg]

    member x.setRegister (reg: Register) (value: MachineInt) : MachineState =
        let registers = x.Registers
        Array.set registers reg value
        { x with Registers = registers }

    member x.setPC (pc : MachineInt) : MachineState =
        { x with PC = pc }

    member x.incPC : MachineState =
        { x with PC = x.PC + 4L }

    member x.getMemory(addr : int64) : byte =
        if Map.containsKey addr x.Memory then
            x.Memory.[addr]
        else
            0uy

    member x.setMemory (addr : int64) (value : byte) : MachineState =
        let mem = x.Memory
//        mem.[addr] = value
//        Map.remove addr mem
//        Array.set mem addr value
        { x with Memory = mem }

    member x.setRunState state =
        { x with RunState = state }

let InitMachineState mem verbosity : MachineState =
    {
        PC           = 0x80000000L
        Registers    = Array.zeroCreate 32
        Memory       = mem
        Verbosity    = verbosity
        RunState     = RunMachineState.NotRun
    }
