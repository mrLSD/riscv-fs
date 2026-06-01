module Tests.unit.app

open System.IO
open Xunit

open ISA.RISCV
open ISA.RISCV.Arch
open ISA.RISCV.MachineState
open ISA.RISCV.CLI

// ELF32 whose PT_LOAD at 0x80000000 is: addi x5,x0,42 ; jal x0,0 (self-loop => Stopped)
let private progElf =
    let prog = [| 0x93uy;0x02uy;0xA0uy;0x02uy; 0x6Fuy;0x00uy;0x00uy;0x00uy |]
    Array.append
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
           0x08uy;0x00uy;0x00uy;0x00uy
           0x08uy;0x00uy;0x00uy;0x00uy
           0x05uy;0x00uy;0x00uy;0x00uy
           0x00uy;0x10uy;0x00uy;0x00uy |]
        prog

let private withElf (f : string -> unit) =
    let path = Path.GetTempFileName()
    try
        File.WriteAllBytes(path, progElf)
        f path
    finally
        File.Delete path

[<Fact>]
let ``Run.Run loads an ELF and runs to Stopped (verbose)`` () =
    withElf (fun path ->
        let cfg = { AppConfig.Default with Arch = Some RV32i; Files = Some [| path |]; Verbosity = Some true }
        let res = Run.Run cfg
        Assert.Equal(RunMachineState.Stopped, res.RunState)
        Assert.Equal(42L, res.getRegister 5))

[<Fact>]
let ``main: help returns 0`` () = Assert.Equal(0, main.main [| "-h" |])

[<Fact>]
let ``main: failed parse returns 0`` () = Assert.Equal(0, main.main [| "-A" |])

[<Fact>]
let ``main: missing required params returns 0`` () = Assert.Equal(0, main.main [| "-v" |])

[<Fact>]
let ``main: full run returns 0`` () =
    withElf (fun path -> Assert.Equal(0, main.main [| "-A"; "rv32i"; path |]))

[<Fact>]
let ``runCycle traps on an illegal instruction`` () =
    let m = (MachineState.InitMachineState Map.empty RV32i false).setRunState RunMachineState.Run
    let m = m.storeMemoryWord 0x80000000L 0L
    let m = Run.runCycle m
    match m.RunState with
    | Trap TrapErrors.InstructionDecode -> ()
    | s -> Assert.True(false, sprintf "%A" s)

[<Fact>]
let ``runSteps aborts a non-terminating program with StepLimit`` () =
    // jal x0,+4 ; jal x0,-4  -> infinite 2-instruction loop (neither targets its own PC)
    let m = (MachineState.InitMachineState Map.empty RV32i false).setRunState RunMachineState.Run
    let m = m.storeMemoryWord 0x80000000L 0x0040006fL
    let m = m.storeMemoryWord 0x80000004L 0xffdff06fL
    Assert.Equal(RunMachineState.Trap TrapErrors.StepLimit, (Run.runSteps 50 m).RunState)

[<Fact>]
let ``main: a malformed ELF is reported without crashing`` () =
    let path = Path.GetTempFileName()
    try
        File.WriteAllBytes(path, [| 0uy; 1uy; 2uy; 3uy |])
        Assert.Equal(0, main.main [| "-A"; "rv32i"; path |])
    finally
        File.Delete path

[<Fact>]
let ``runSteps completes when the budget equals the instruction count`` () =
    let prog = [ 0x00500093; 0x00700113; 0x002081b3; 0x00100073 ]  // 4 instrs, ebreak last
    let m = (MachineState.InitMachineState Map.empty RV32i false).setRunState RunMachineState.Run
    let m = prog |> List.mapi (fun i w -> (0x80000000L + int64 (i * 4), int64 w))
                 |> List.fold (fun (s : MachineState) (a, w) -> s.storeMemoryWord a w) m
    Assert.Equal(RunMachineState.Trap TrapErrors.EBreak, (Run.runSteps 4 m).RunState)
    Assert.Equal(RunMachineState.Trap TrapErrors.StepLimit, (Run.runSteps 3 m).RunState)

[<Fact>]
let ``runSteps 0 immediately hits StepLimit`` () =
    let m = (MachineState.InitMachineState Map.empty RV32i false).setRunState RunMachineState.Run
    Assert.Equal(RunMachineState.Trap TrapErrors.StepLimit, (Run.runSteps 0 m).RunState)
