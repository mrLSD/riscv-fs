module ISA.RISCV.MachineState

open Microsoft.FSharp.Collections
open ISA.RISCV.Arch
open ISA.RISCV.Utils.Bits

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
        if reg = 0 then
            0L // x0 always 0
        else
            x.Registers.[reg]

    member x.setRegister (reg: Register) (value: MachineInt) : MachineState =
        let registers = x.Registers
        // Check x0 register that always 0
        let value = if reg = 0 then 0L else value
        Array.set registers reg (x.alignByArch value)
        { x with Registers = registers }

    member x.setPC (pc : MachineInt) : MachineState =
        { x with PC = x.alignByArchUnsign pc }

    member x.incPC : MachineState =
        x.setPC (x.PC + 4L)

    member x.getMemory(addr : int64) =
        let addr = x.alignByArchUnsign addr
        if Map.containsKey addr x.Memory then
            Some(x.Memory.[addr])
        else
            None

    member x.setMemoryByte (addr : int64) (value : byte) : MachineState =
        let addr = x.alignByArchUnsign addr
        let mem = Map.add addr (byte value) x.Memory
        { x with Memory = mem }
    
    member x.storeMemoryByte (addr : MachineInt) (value : MachineInt) : MachineState =
        let nBytes = 1
        Array.fold (fun (ms : MachineState) (addr, data) -> ms.setMemoryByte addr data) x
            [| for i in 0..(nBytes-1) -> (addr+(int64 i), byte (value.bitSlice (i*8+7) (i*8) )) |]
    
    member x.storeMemoryHalfWord (addr : MachineInt) (value : MachineInt) : MachineState =
        let nBytes = 2
        Array.fold (fun (ms : MachineState) (addr, data) -> ms.setMemoryByte addr data) x
            [| for i in 0..(nBytes-1) -> (addr+(int64 i), byte (value.bitSlice (i*8+7) (i*8) )) |]
    
    member x.storeMemoryWord (addr : MachineInt) (value : MachineInt) : MachineState =
        let nBytes = 4
        Array.fold (fun (ms : MachineState) (addr, data) -> ms.setMemoryByte addr data) x
            [| for i in 0..(nBytes-1) -> (addr+(int64 i), byte (value.bitSlice (i*8+7) (i*8) )) |]

    member x.storeMemoryDoubleWord (addr : MachineInt) (value : MachineInt) : MachineState =
        let nBytes = 8
        Array.fold (fun (ms : MachineState) (addr, data) -> ms.setMemoryByte addr data) x
            [| for i in 0..(nBytes-1) -> (addr+(int64 i), byte (value.bitSlice (i*8+7) (i*8) )) |]
        
    member x.setRunState state =
        { x with RunState = state }
    member x.alignByArch (value : int64) =
        // if x32 Arch - align it to x32
        // and then convert again to int64
        match x.Arch.archBits with
        | Architecture.RV32 -> int64(int32 value)
        | _ -> value

    member x.alignByArchUnsign (value : int64) =
        // if x32 Arch - align it to x32
        // and then convert again to int64
        match x.Arch.archBits with
        | Architecture.RV32 -> int64(uint32 value)
        | _ -> int64(uint64 value)

let InitMachineState mem arch verbosity : MachineState =
    {
        PC           = 0x80000000L
        Registers    = Array.zeroCreate 32
        Memory       = mem
        Arch         = arch
        Verbosity    = verbosity
        RunState     = RunMachineState.NotRun
    }
