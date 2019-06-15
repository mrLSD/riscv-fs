module ISA.RISCV.MachineState

open Microsoft.FSharp.Collections
open ISA.RISCV.Arch

type MachineState = {
        PC:         int32
        Registers:  Register array
        Memory:     int8 array
    } with
    member x.getRegister(reg: int32) : Register =
        x.Registers.[reg]

    member x.setRegister (reg: int32) (value: Register) : MachineState =
        let registers = x.Registers
        Array.set registers reg value
        { x with Registers = registers }

    member x.setPC (pc : int32) : MachineState =
        { x with PC = pc }

    member x.getMemory(addr : int32) : int8 =
        if addr >= x.Memory.Length then
            0y
        else
            x.Memory.[addr]

    member x.setMemory (addr : int32) (value : sbyte) : MachineState =
        let mem = x.Memory
        Array.set mem addr value
        { x with Memory = mem }
