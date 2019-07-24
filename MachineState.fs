module ISA.RISCV.MachineState

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
        Arch:       Architecture
        RunState:   RunMachineState
    } with
    member x.getRegister(reg: Register) : MachineInt =
        x.Registers.[reg]

    member x.setRegister (reg: Register) (value: MachineInt) : MachineState =
        let registers = x.Registers
        Array.set registers reg (x.alignByArch value)
        { x with Registers = registers }

    member x.setPC (pc : MachineInt) : MachineState =
        { x with PC = x.alignByArch pc }

    member x.incPC : MachineState =
        { x with PC = x.alignByArch (x.PC + 4L) }

    member x.getMemory(addr : int64) =
        let addr = x.alignByArch addr
        if Map.containsKey addr x.Memory then
            Some(x.Memory.[addr])
        else
            None

    member x.setMemoryByte (addr : int64) (value : byte) : MachineState =
        let addr = x.alignByArch addr
        let mem = Map.add addr (byte value) x.Memory
        { x with Memory = mem }

    member x.setRunState state =
        { x with RunState = state }
    member x.alignByArch (value : int64) =
        // if x32 Arch - align it to x32
        // and then convert again to int64
        match x.Arch.archBits with
        | Architecture.RV32 -> int64( int32(value) )
        | _ -> value

let InitMachineState mem arch verbosity : MachineState =
    {
        PC           = 0x80000000L
        Registers    = Array.zeroCreate 32
        Memory       = mem
        Arch         = arch
        Verbosity    = verbosity
        RunState     = RunMachineState.NotRun
    }
