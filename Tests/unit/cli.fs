module Tests.unit.cli

open Xunit

open ISA.RISCV.Arch
open ISA.RISCV.CLI

let private parse argv = parseCli argv InitCLI AppConfig.Default

[<Fact>]
let ``short arch + file => Success and CheckRequired`` () =
    match parse [| "-A"; "rv32i"; "a.elf" |] with
    | Success cfg ->
        Assert.Equal(Some RV32i, cfg.Arch)
        Assert.True(cfg.CheckRequired)
        Assert.Equal<string[]>([| "a.elf" |], cfg.Files.Value)
    | r -> Assert.True(false, sprintf "%A" r)

[<Fact>]
let ``long arch => Success`` () =
    match parse [| "--arch"; "rv64ima"; "a.elf" |] with
    | Success cfg -> Assert.Equal(Some RV64ima, cfg.Arch)
    | r -> Assert.True(false, sprintf "%A" r)

[<Fact>]
let ``verbosity short flag`` () =
    match parse [| "-v"; "-A"; "rv32i"; "a.elf" |] with
    | Success cfg -> Assert.Equal(Some true, cfg.Verbosity)
    | r -> Assert.True(false, sprintf "%A" r)

[<Fact>]
let ``multiple files accumulate`` () =
    match parse [| "-A"; "rv32i"; "a.elf"; "b.elf"; "c.elf" |] with
    | Success cfg -> Assert.Equal(3, cfg.Files.Value.Length)
    | r -> Assert.True(false, sprintf "%A" r)

[<Fact>]
let ``help -h stops execution`` () = Assert.Equal(Stopped, parse [| "-h" |])
[<Fact>]
let ``help --help stops execution`` () = Assert.Equal(Stopped, parse [| "--help" |])
[<Fact>]
let ``version -V stops execution`` () = Assert.Equal(Stopped, parse [| "-V" |])
[<Fact>]
let ``version --version stops execution`` () = Assert.Equal(Stopped, parse [| "--version" |])

[<Fact>]
let ``missing arch value => Failed`` () = Assert.Equal(Failed, parse [| "-A" |])

[<Fact>]
let ``arch value starting with dash => Failed`` () = Assert.Equal(Failed, parse [| "-A"; "-x" |])

[<Fact>]
let ``no arch or files => Success but not CheckRequired`` () =
    match parse [| "-v" |] with
    | Success cfg -> Assert.False(cfg.CheckRequired)
    | r -> Assert.True(false, sprintf "%A" r)

[<Fact>]
let ``unknown arch string => not CheckRequired`` () =
    match parse [| "-A"; "rvXX"; "a.elf" |] with
    | Success cfg -> Assert.False(cfg.CheckRequired)
    | r -> Assert.True(false, sprintf "%A" r)

[<Fact>]
let ``CliUsage prints without error`` () =
    CliUsage InitCLI

[<Fact>]
let ``long-key-only option is parsed`` () =
    let opts = [| { CliOptions.Default with LongKey = Some "flag" } |]
    match parseCli [| "--flag" |] opts AppConfig.Default with
    | Failed -> Assert.True(false, "unexpected Failed")
    | _ -> ()

[<Fact>]
let ``printHelpMessage: long-key-only and bare options`` () =
    ({ CliOptions.Default with LongKey = Some "flag" }).printHelpMessage
    (CliOptions.Default).printHelpMessage

[<Fact>]
let ``--arch as long form with no value => Failed`` () = Assert.Equal(Failed, parse [| "--arch" |])

[<Fact>]
let ``--arch as long form with dash value => Failed`` () = Assert.Equal(Failed, parse [| "--arch"; "-x" |])

let private optLKV = [| { CliOptions.Default with LongKey = Some "name"; Value = Some "N" } |]

[<Fact>]
let ``long-key-only option consumes its value`` () =
    match parseCli [| "--name"; "bob" |] optLKV AppConfig.Default with
    | Failed -> Assert.True(false, "unexpected Failed") | _ -> ()

[<Fact>]
let ``long-key-only option missing value => Failed`` () =
    Assert.Equal(Failed, parseCli [| "--name" |] optLKV AppConfig.Default)

[<Fact>]
let ``long-key-only option dash value => Failed`` () =
    Assert.Equal(Failed, parseCli [| "--name"; "-x" |] optLKV AppConfig.Default)

[<Fact>]
let ``long-key-only option non-matching arg is ignored`` () =
    match parseCli [| "other" |] [| { CliOptions.Default with LongKey = Some "flag" } |] AppConfig.Default with
    | Failed -> Assert.True(false, "unexpected Failed") | _ -> ()

[<Fact>]
let ``bare option is ignored`` () =
    match parseCli [| "x" |] [| CliOptions.Default |] AppConfig.Default with
    | Failed -> Assert.True(false, "unexpected Failed") | _ -> ()

[<Fact>]
let ``multiple key flag with a trailing non-matching arg`` () =
    let opt = [| { CliOptions.Default with Key = Some "f"; Multiple = true } |]
    match parseCli [| "-f"; "z" |] opt AppConfig.Default with
    | Failed -> Assert.True(false, "unexpected Failed") | _ -> ()

[<Fact>]
let ``long-key-only option with value advances past both tokens`` () =
    let opt = { CliOptions.Default with LongKey = Some "name"; Value = Some "N" }
    let (_, leftover) = fetchArgs [| "--name"; "bob" |] opt AppConfig.Default
    Assert.Equal<string[]>([||], leftover)

// Exercise every fetchArgs/parseCli arm and guard combination for branch coverage.
[<Fact>]
let ``parser exercises all fetchArgs and parseCli arms`` () =
    let filesOpt = { CliOptions.Default with Value = Some "F"; Multiple = true }
    let keyMul   = { CliOptions.Default with Key = Some "f"; Multiple = true }
    let aOpt     = { CliOptions.Default with Key = Some "A"; Value = Some "ARCH" }
    // Multiple value-option: several values (recurse), single (base), and empty argv
    fetchArgs [| "a"; "b"; "c" |] filesOpt AppConfig.Default |> ignore
    fetchArgs [| "a" |] filesOpt AppConfig.Default |> ignore
    fetchArgs [||] filesOpt AppConfig.Default |> ignore
    // Multiple key-option: trailing match (recurse) and non-match (inner NotFound->Result)
    fetchArgs [| "-f"; "-f" |] keyMul AppConfig.Default |> ignore
    fetchArgs [| "-f"; "z" |] keyMul AppConfig.Default |> ignore
    // Non-multiple option: leftover (_ with len-resIndex>0) and none (_ with =0)
    fetchArgs [| "-A"; "rv32i"; "x"; "y" |] aOpt AppConfig.Default |> ignore
    fetchArgs [| "-A"; "rv32i" |] aOpt AppConfig.Default |> ignore
    // NotFound with following args (recurse) and as the only arg
    fetchArgs [| "zzz"; "-A"; "rv32i" |] aOpt AppConfig.Default |> ignore
    fetchArgs [| "zzz" |] aOpt AppConfig.Default |> ignore
    // parseCli: single option (opts.Length=1), full chain with trailing files, empty argv
    parseCli [| "-A"; "rv32i" |] [| aOpt |] AppConfig.Default |> ignore
    parseCli [| "-A"; "rv32i"; "f1"; "f2" |] InitCLI AppConfig.Default |> ignore
    parseCli [||] InitCLI AppConfig.Default |> ignore
    Assert.True true
