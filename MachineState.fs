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
        Reservation: (int64 * int) option
        InstrLen:   int
    } with
    member x.getRegister(reg: Register) : MachineInt =
        if reg = 0 then
            0L // x0 always 0
        else
            x.Registers.[reg]

    member x.setRegister (reg: Register) (value: MachineInt) : MachineState =
        // Copy so a prior state keeps its registers (true immutability).
        let registers = Array.copy x.Registers
        // Check x0 register that always 0
        let value = if reg = 0 then 0L else value
        Array.set registers reg (x.alignByArch value)
        { x with Registers = registers }

    member x.setPC (pc : MachineInt) : MachineState =
        { x with PC = x.alignByArchUnsign pc }

    member x.incPC : MachineState =
        x.setPC (x.PC + int64 x.InstrLen)
    // Instruction alignment (IALIGN): 2 bytes when C is supported, else 4.
    member x.instrAlign : int64 =
        if x.Arch.hasC then 2L else 4L

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
    
    // Store `nBytes` little-endian bytes of `value` starting at `addr`.
    // An explicit recursive loop keeps the only branch (i >= nBytes) fully
    // exercised for any nBytes >= 1 (both the continue and stop sides are hit).
    member private x.storeBytes (nBytes : int) (addr : MachineInt) (value : MachineInt) : MachineState =
        let rec loop (ms : MachineState) i =
            if i >= nBytes then ms
            else loop (ms.setMemoryByte (addr + int64 i) (byte (value.bitSlice (i*8+7) (i*8)))) (i + 1)
        loop x 0

    member x.storeMemoryByte (addr : MachineInt) (value : MachineInt) : MachineState =
        x.storeBytes 1 addr value
    
    member x.storeMemoryHalfWord (addr : MachineInt) (value : MachineInt) : MachineState =
        x.storeBytes 2 addr value
    
    member x.storeMemoryWord (addr : MachineInt) (value : MachineInt) : MachineState =
        x.storeBytes 4 addr value

    member x.storeMemoryDoubleWord (addr : MachineInt) (value : MachineInt) : MachineState =
        x.storeBytes 8 addr value

    // Symmetric with storeBytes: load `nBytes` little-endian bytes starting at `addr`,
    // normalizing EACH byte address to XLEN (alignByArchUnsign) just as the store path
    // does, so a multi-byte access that wraps past 2^XLEN reads the same Map keys the
    // matching store wrote. Returns None (=> the caller raises a MemAddress trap) if any
    // byte is unmapped.
    member private x.loadBytes (nBytes : int) (addr : MachineInt) : MachineInt option =
        let rec loop i acc =
            if i >= nBytes then Some acc
            else
                let a = x.alignByArchUnsign (addr + int64 i)
                match Map.tryFind a x.Memory with
                | None   -> None
                | Some b -> loop (i + 1) (acc ||| (int64 b <<< (i * 8)))
        loop 0 0L

    member x.loadMemoryByte (addr : MachineInt) : int8 option =
        x.loadBytes 1 addr |> Option.map int8
    member x.loadMemoryHalfWord (addr : MachineInt) : int16 option =
        x.loadBytes 2 addr |> Option.map int16
    member x.loadMemoryWord (addr : MachineInt) : int32 option =
        x.loadBytes 4 addr |> Option.map int32
    member x.loadMemoryDoubleWord (addr : MachineInt) : int64 option =
        x.loadBytes 8 addr

    member x.setRunState state =
        { x with RunState = state }
    // Reservation tracks both the address and the access width (bytes): an SC
    // succeeds only when it pairs with an LR of the same address and width.
    member x.setReservation (addr : MachineInt) (width : int) : MachineState =
        { x with Reservation = Some (addr, width) }
    member x.clearReservation : MachineState =
        { x with Reservation = None }
    // Represent a signed value at the register width (XLEN):
    // RV32 keeps the low 32 bits sign-extended; RV64 is unchanged.
    member x.alignByArch (value : int64) =
        match x.Arch.archBits with
        | Architecture.RV32 -> int64(int32 value)
        | _ -> value

    // Represent an unsigned value at the width (XLEN), used for PC/addresses:
    // RV32 keeps the low 32 bits zero-extended; RV64 is unchanged.
    member x.alignByArchUnsign (value : int64) =
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
        Reservation  = None
        InstrLen     = 4
    }
