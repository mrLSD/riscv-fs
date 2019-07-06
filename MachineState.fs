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
        PC:         uint32
        Registers:  Register array
        Memory:     Map<uint32, byte>
        Verbosity:  bool
        RunState:   RunMachineState
    } with
    member x.getRegister(reg: int32) : Register =
        x.Registers.[reg]

    member x.setRegister (reg: int32) (value: Register) : MachineState =
        let registers = x.Registers
        Array.set registers reg value
        { x with Registers = registers }

    member x.setPC (pc : uint32) : MachineState =
        { x with PC = pc }

    member x.incPC (pc : uint32) : MachineState =
        { x with PC = x.PC + pc }

    member x.getMemory(addr : uint32) : byte =
        if Map.containsKey addr x.Memory then
            x.Memory.[addr]
        else
            0uy

    member x.setMemory (addr : uint32) (value : byte) : MachineState =
        let mem = x.Memory
//        mem.[addr] = value
//        Map.remove addr mem
//        Array.set mem addr value
        { x with Memory = mem }

    member x.setRunState state =
        { x with RunState = state }

let InitMachineState mem verbosity : MachineState =
    {
        PC           = 0x80000000u
        Registers    = Array.zeroCreate 32
        Memory       = mem
        Verbosity    = verbosity
        RunState     = RunMachineState.NotRun
    }
