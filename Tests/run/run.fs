module Tests.run.run

open System.IO
open Xunit

open ISA.RISCV
open ISA.RISCV.Arch
open ISA.RISCV.MachineState

// Minimal ELF images with a single PT_LOAD segment: 4 data bytes (DE AD BE EF)
// at 0x80000000 and memsz = 6 (two zero-filled .bss bytes).
let private elf32 =
    [| 0x7Fuy;0x45uy;0x4Cuy;0x46uy; 0x01uy;0x01uy;0x01uy;0x00uy; 0uy;0uy;0uy;0uy;0uy;0uy;0uy;0uy
       0x02uy;0x00uy; 0xF3uy;0x00uy; 0x01uy;0x00uy;0x00uy;0x00uy
       0x00uy;0x00uy;0x00uy;0x80uy
       0x34uy;0x00uy;0x00uy;0x00uy
       0x00uy;0x00uy;0x00uy;0x00uy
       0x00uy;0x00uy;0x00uy;0x00uy
       0x34uy;0x00uy; 0x20uy;0x00uy; 0x01uy;0x00uy
       0x00uy;0x00uy; 0x00uy;0x00uy; 0x00uy;0x00uy
       0x01uy;0x00uy;0x00uy;0x00uy
       0x54uy;0x00uy;0x00uy;0x00uy
       0x00uy;0x00uy;0x00uy;0x80uy
       0x00uy;0x00uy;0x00uy;0x80uy
       0x04uy;0x00uy;0x00uy;0x00uy
       0x06uy;0x00uy;0x00uy;0x00uy
       0x05uy;0x00uy;0x00uy;0x00uy
       0x00uy;0x10uy;0x00uy;0x00uy
       0xDEuy;0xADuy;0xBEuy;0xEFuy |]

let private elf64 =
    [| 0x7Fuy;0x45uy;0x4Cuy;0x46uy; 0x02uy;0x01uy;0x01uy;0x00uy; 0uy;0uy;0uy;0uy;0uy;0uy;0uy;0uy
       0x02uy;0x00uy; 0xF3uy;0x00uy; 0x01uy;0x00uy;0x00uy;0x00uy
       0x00uy;0x00uy;0x00uy;0x80uy;0x00uy;0x00uy;0x00uy;0x00uy
       0x40uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy
       0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy
       0x00uy;0x00uy;0x00uy;0x00uy
       0x40uy;0x00uy; 0x38uy;0x00uy; 0x01uy;0x00uy
       0x00uy;0x00uy; 0x00uy;0x00uy; 0x00uy;0x00uy
       0x01uy;0x00uy;0x00uy;0x00uy
       0x05uy;0x00uy;0x00uy;0x00uy
       0x78uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy
       0x00uy;0x00uy;0x00uy;0x80uy;0x00uy;0x00uy;0x00uy;0x00uy
       0x00uy;0x00uy;0x00uy;0x80uy;0x00uy;0x00uy;0x00uy;0x00uy
       0x04uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy
       0x06uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy
       0x00uy;0x10uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy;0x00uy
       0xDEuy;0xADuy;0xBEuy;0xEFuy |]

let private withTempElf (bytes : byte[]) (f : string -> unit) =
    let path = Path.GetTempFileName()
    try
        File.WriteAllBytes(path, bytes)
        f path
    finally
        File.Delete path

// ---- M4: ELF loader loads data segments (not just executable) for both classes ----
[<Fact>]
let ``readElfFile loads a 32-bit PT_LOAD segment and zero-fills bss`` () =
    withTempElf elf32 (fun path ->
        let mem = Run.readElfFile path
        Assert.Equal(6, Map.count mem)
        Assert.Equal(0xDEuy, mem.[0x80000000L])
        Assert.Equal(0xADuy, mem.[0x80000001L])
        Assert.Equal(0xBEuy, mem.[0x80000002L])
        Assert.Equal(0xEFuy, mem.[0x80000003L])
        Assert.Equal(0x00uy, mem.[0x80000004L])
        Assert.Equal(0x00uy, mem.[0x80000005L]))

[<Fact>]
let ``readElfFile loads a 64-bit PT_LOAD segment`` () =
    withTempElf elf64 (fun path ->
        let mem = Run.readElfFile path
        Assert.Equal(6, Map.count mem)
        Assert.Equal(0xDEuy, mem.[0x80000000L])
        Assert.Equal(0xEFuy, mem.[0x80000003L])
        Assert.Equal(0x00uy, mem.[0x80000005L]))

// ---- M5: full fetch/decode/execute/trap loop ----
let private loadProgram (words : int list) =
    let m = (MachineState.InitMachineState Map.empty RV32i false).setRunState RunMachineState.Run
    words
    |> List.mapi (fun i w -> (0x80000000L + int64 (i * 4), int64 w))
    |> List.fold (fun (s : MachineState) (a, w) -> s.storeMemoryWord a w) m

[<Fact>]
let ``runCycle executes a program and stops on EBREAK`` () =
    // addi x1,x0,5 ; addi x2,x0,7 ; add x3,x1,x2 ; ebreak
    let m = loadProgram [ 0x00500093; 0x00700113; 0x002081b3; 0x00100073 ]
    let m = Run.runCycle m
    Assert.Equal(12L, m.getRegister 3)
    Assert.Equal(RunMachineState.Trap TrapErrors.EBreak, m.RunState)

[<Fact>]
let ``runCycle traps when PC runs into unmapped memory`` () =
    // addi x1,x0,5 ; (no instruction at the next PC)
    let m = loadProgram [ 0x00500093 ]
    let m = Run.runCycle m
    Assert.Equal(5L, m.getRegister 1)
    match m.RunState with
    | Trap (InstructionFetch _) -> ()
    | s -> Assert.True(false, sprintf "expected InstructionFetch trap, got %A" s)

[<Fact>]
let ``readElfFile throws on a non-ELF file`` () =
    let path = Path.GetTempFileName()
    try
        File.WriteAllBytes(path, [| 0uy; 1uy; 2uy; 3uy |])
        let threw = try (Run.readElfFile path |> ignore; false) with _ -> true
        Assert.True(threw)
    finally
        File.Delete path
