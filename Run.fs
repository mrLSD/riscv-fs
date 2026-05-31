module ISA.RISCV.Run

open System

open ELFSharp.ELF
open ELFSharp.ELF.Segments

open ISA.RISCV
open ISA.RISCV.MachineState
open ISA.RISCV.Utils.Bits
open ISA.RISCV.Arch
open ISA.RISCV.CLI

// Get registers state
let verbosityMessageRegisters (mstate : MachineState) =
    printfn "Not zero Registers: "
    for x in 0..31 do
        if mstate.Registers.[x] <> 0L then
            let value = sprintf "0x%x" mstate.Registers.[x]
            printfn "%s" (String.Format("\tx{0, -3}{1}", x, value))

/// Read Elf data content to Map data with format: [address, dataByte].
/// Loads every PT_LOAD segment (code, data, zero-filled bss) for 32- and 64-bit ELF.
let readElfFile (file : string) : Map<int64, byte> =
    let toPairs (vaddr : int64) (bytes : byte array) =
        Array.mapi (fun i b -> (vaddr + int64 i, b)) bytes
    match ELFReader.CheckELFType file with
    | Class.Bit64 ->
        let elf = ELFReader.Load<uint64> file
        elf.Segments
        |> Seq.filter (fun s -> s.Type = SegmentType.Load)
        |> Seq.collect (fun s -> toPairs (int64 s.Address) (s.GetMemoryContents()))
        |> Map.ofSeq
    | _ ->
        let elf = ELFReader.Load<uint32> file
        elf.Segments
        |> Seq.filter (fun s -> s.Type = SegmentType.Load)
        |> Seq.collect (fun s -> toPairs (int64 s.Address) (s.GetMemoryContents()))
        |> Map.ofSeq

// Get instruction from current Machine State that related to
// current PC as memory address for loading instruction data for Decoding
let fetchInstruction (mstate : MachineState) : InstrField option =
    loadWord mstate.Memory mstate.PC

// Basic RISC-V run life cycle. Represent Finite State Machine (FSM)
let rec runCycle (mstate : MachineState) =
    let instr = fetchInstruction mstate

    let mstate =
        match instr with
        | None -> mstate.setRunState (Trap (InstructionFetch mstate.PC))
        | _ ->
            let instrValue = instr.Value
            let executor = Decoder.Decode mstate instrValue

            match executor with
            | None -> mstate.setRunState (Trap TrapErrors.InstructionDecode)
            | _ ->
// TODO: Change that logic
//                if mstate.Verbosity then
//                    verbosityMessage instr.Value decodedInstr mstate
                // Executor for specific Instruction Set
                // that was detected in Decoder
                let Executor = executor.Value
                // Execute current Instruction
                Executor mstate
    match mstate.RunState with
    | Trap _ -> mstate
    | RunMachineState.Stopped ->
        verbosityMessageRegisters mstate
        mstate
    | _ -> runCycle mstate

// Main application Run logic
let Run (cfg : AppConfig) =
    let data = readElfFile cfg.Files.Value.[0]
    let mstate = InitMachineState data cfg.Arch.Value cfg.Verbosity.Value
    runCycle mstate
