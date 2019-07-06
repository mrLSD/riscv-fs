module ISA.RISCV.CLI
#nowarn "40"

open System

let version = "v0.1.0"
let author = "(c) 20019 by Evgeny Ukhanov"
let about = sprintf "RISC-V Simulator for Formal RISC-V ISA implementation\n%s %s" version author

type AppConfig = {
        Verbosity: bool option
        Arch:      string option
        Files:     string[] option
    } with
    static member Default =
        {
            Verbosity = Some(false)
            Arch      = None
            Files     = None
        }
    member x.CheckRequired =
        if x.Files.IsNone || x.Arch.IsNone then
            false
        else
            true

type CliResult =
    | Result of AppConfig
    | Error
    | NotFound of AppConfig

type CliStatus =
    | Success of AppConfig
    | Stopped
    | Failed

type CliOptions = {
        Key:           string option
        LongKey:       string option
        Value:         string option
        Multiple:      bool
        HelpMessage:   string
        StopExecution: bool
        Handler:       (string -> AppConfig -> AppConfig)
    } with
    static member Default =
        {
            Key           = None
            LongKey       = None
            Value         = None
            Multiple      = false
            HelpMessage   = ""
            StopExecution = false
            Handler       = fun _ cfg -> cfg
        }
    member x.printHelpMessage =
        let msg = if x.Key.IsSome && x.LongKey.IsSome then
                      sprintf "-%s, --%s" x.Key.Value x.LongKey.Value
                  else if x.Key.IsSome then
                      sprintf "-%s\t" x.Key.Value
                  else if x.LongKey.IsSome then
                      sprintf "--%s\t" x.LongKey.Value
                  else if x.Value.IsSome then
                      sprintf "<%s>\t" x.Value.Value
                  else
                      ""
        printfn "%s" (String.Format("{0,-5}{1,-20} {2}", "", msg, x.HelpMessage))

let CliUsage (cliArgs : CliOptions []) =
    printfn "%s" about
    printfn "%s" (String.Format("USAGE:\n{0,-5}risc-v [OPTIONS] file...\nOPTIONS", ""))
    for arg in cliArgs do
        arg.printHelpMessage

let rec fetchArgs (argv : string[]) (opts : CliOptions) (cfg : AppConfig) =
    if argv.Length < 1 then
        (NotFound(cfg), argv)
    else
        let arg = argv.[0]
        let (cfgRes, resIndex) =
            if opts.Key.IsSome then
                if  sprintf "-%s" opts.Key.Value = arg then
                    if opts.Value.IsSome then
                        if argv.Length > 1 then
                            let arg2 = argv.[1]
                            // Check is value not argument parameter
                            if "-" = arg2.[..0] then
                                (Error, 0)
                            else
                                let cfg = opts.Handler arg2 cfg
                                (Result(cfg), 2)
                        else
                            (Error, 0)
                    else
                        let cfg = opts.Handler arg cfg
                        (Result(cfg), 1)
                else if opts.LongKey.IsSome && sprintf "--%s" opts.LongKey.Value = arg then
                    if opts.Value.IsSome then
                        if argv.Length > 1 then
                            let arg2 = argv.[1]
                            if "-" = arg2.[..0] then
                                (Error, 0)
                            else
                                let cfg = opts.Handler arg2 cfg
                                (Result(cfg), 2)
                        else
                            (Error, 0)
                    else
                        let cfg = opts.Handler arg cfg
                        (Result(cfg), 1)
                else
                    (NotFound(cfg), 0)

            else if opts.LongKey.IsSome then
                if sprintf "--%s" opts.LongKey.Value = arg then
                    if opts.Value.IsSome then
                        if argv.Length > 1 then
                            let arg2 = argv.[1]
                            if "-" = arg2.[..0] then
                                (Error, 0)
                            else
                                let cfg = opts.Handler arg2 cfg
                                (Result(cfg), 0)
                        else
                            (Error, 0)
                    else
                        let cfg = opts.Handler arg cfg
                        (Result(cfg), 1)
                else
                    (NotFound(cfg), 0)

            else if opts.Value.IsSome then
                let cfg = opts.Handler arg cfg
                (Result(cfg), 1)
            else
                (NotFound(cfg), 0)

        match cfgRes with
        | Result(res) when opts.Multiple && argv.Length - resIndex > 0 ->
            let cfgRes = fetchArgs argv.[resIndex..] opts res
            // If NotFound for that branch loop -
            // redeclare to Result type
            let resValue = match cfgRes with
                           | (NotFound(x), newArgs) -> (Result(x), newArgs)
                           | _ -> cfgRes
            resValue
        | NotFound(res) when argv.Length > 0 ->
            let (cfgRes, changedArgs) = fetchArgs argv.[1..] opts res
            (cfgRes, Array.append [|arg|] changedArgs)
        | _ ->
            if argv.Length - resIndex > 0 then
                (cfgRes, argv.[resIndex..])
            else
                (cfgRes, [||])

// Parse CLI with specific params
let rec parseCli (argv : string[]) (opts : CliOptions[]) (cfg : AppConfig) =
    if opts.Length < 1 then
        Success(cfg)
    else
        let opt = opts.[0]
        let opts = if opts.Length > 1 then opts.[1..] else [||]
        let (resCfg, newArgv) = fetchArgs argv opt cfg
        match resCfg with
        | Error ->
            Failed
        | NotFound(cfg) ->
            parseCli newArgv opts cfg
        | Result(cfg) ->
            if opt.StopExecution then
                Stopped
            else
                parseCli newArgv opts cfg

// Init CLI options and arguments
let rec InitCLI =
    [|
        { CliOptions.Default with
            Key =  Some("A");
            LongKey = Some("arch");
            Value = Some("ARCH")
            HelpMessage = "RISC-V architecture. Available: rv32i. Default: rv32i"
            Handler =
                fun arg cfg ->
                    { cfg with
                        AppConfig.Arch = Some(arg)
                    }
        };
        { CliOptions.Default with
            Key =  Some("v");
            HelpMessage = "Verbosity output"
            Handler =
                fun arg cfg ->
                { cfg with
                    AppConfig.Verbosity = Some(true)
                }
        };
        { CliOptions.Default with
            Key =  Some("h");
            LongKey = Some("help");
            HelpMessage = "Print help message"
            StopExecution = true
            Handler =
                fun arg cfg ->
                    CliUsage InitCLI
                    cfg
        };
        { CliOptions.Default with
            Key =  Some("V");
            LongKey = Some("version");
            HelpMessage = "Application version"
            StopExecution = true
            Handler =
                fun _ cfg ->
                    printfn "%s" about
                    cfg
        };
        { CliOptions.Default with
            HelpMessage = "Files to executions"
            Value = Some("FILE")
            Multiple = true
            Handler =
                fun arg cfg ->
                    let res = if cfg.Files.IsSome then cfg.Files.Value else [||]
                    { cfg with
                        AppConfig.Files = Some(Array.append res [| arg |])
                    }

        };
    |]