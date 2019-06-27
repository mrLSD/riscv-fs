module ISA.RISCV.CLI

open ISA.RISCV
open System

let version = "v0.1.0"
let author = "(c) 20019 by Evgeny Ukhanov"
let about = sprintf "RISC-V Simulator for Formal RISC-V ISA implementation\n%s %s" version author

type AppConfig = {
        Verbosity: bool option
        Arch:      string[]
        Files:     string[]
    } with
    static member Default =
        {
            Verbosity = None
            Arch      = [||]
            Files     = [||]
        }

type CliResult =
    | Result of AppConfig
    | Error
    | NotFound of AppConfig

type CliOptions = {
        Key:         string option
        LongKey:     string option
        Value:       string option
        Multiple:    bool
        HelpMessage: string
        Required:    bool
        IsFound:     bool
        Handler:     (string -> AppConfig -> AppConfig)
    } with
    static member Default =
        {
            Key         = None
            LongKey     = None
            Value       = None
            Multiple    = false
            HelpMessage = ""
            Required    = false
            IsFound     = false
            Handler     = fun _ cfg -> cfg
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

let rec fetchArgs (argv : string[]) (opts : CliOptions) (cfg : AppConfig) (index : int) =
    if argv.Length < 1 then
        (NotFound(cfg), [||])
    else
        let arg = argv.[0]
        let res =
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
                        printfn "Key: %s | %A" opts.Key.Value cfg
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
                        printfn "LongKey: %s | %A" opts.LongKey.Value cfg
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
                                printfn "LongKey: %s | %A" opts.LongKey.Value cfg
                                (Result(cfg), 2)
                        else
                            (Error, 0)
                    else
                        let cfg = opts.Handler arg cfg
                        printfn "LongKey: %s | %A" opts.LongKey.Value cfg
                        (Result(cfg), 1)
                else
                    (NotFound(cfg), 0)

            else if opts.Value.IsSome then
                let cfg = opts.Handler arg cfg
                printfn "Value: %s | %A" opts.Value.Value cfg
                (Result(cfg), 1)
            else
                (NotFound(cfg), 0)

        let (cfgRes, resIndex) = res
        let endIndex = if index <= index + resIndex - 1 then index + resIndex - 1 else index
        printfn "INDEX %A" index
        match cfgRes with
        | Result(res) when opts.Multiple && argv.Length - resIndex > 0 ->
                printfn "RES: %A" res
                let (cfgRes, resIndexes) = fetchArgs argv.[resIndex..] opts res (index + resIndex)
                let indexes = Array.append resIndexes [| index .. endIndex |]
                // If NotFound for that branch loop -
                // redeclare to Result type
                let resValue = match cfgRes with
                               | NotFound(x) -> Result(x)
                               | _ -> cfgRes
                (resValue, indexes)
        | NotFound(res) when argv.Length - resIndex > 0 ->
                fetchArgs argv.[1..] opts res (index + 1)
        | _ ->
                (cfgRes, [| index .. endIndex |])

let InitCLI (argv : string[]) =
    let opts = [|
        { CliOptions.Default with
            Key =  Some("A");
            LongKey = Some("arch");
            Value = Some("ARCH")
            Multiple = true
            HelpMessage = "RISC-V architecture. Available: rv32i. Default: rv32i"
            Handler =
                fun arg cfg ->
                { cfg with
                    AppConfig.Arch = Array.append cfg.Arch [| arg |]
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
            Handler =
                fun arg cfg ->
                    cfg
        };
        { CliOptions.Default with
            Key =  Some("V");
            LongKey = Some("version");
            HelpMessage = "Application version"
            Handler =
                fun _ cfg ->
                    printfn "%s" about
                    cfg
        };
    |]
    let appCfg = AppConfig.Default

    for opt in opts do
        printfn "%A\n" opt
        let res = fetchArgs argv opt appCfg 0
        printfn "Parse res: %A\n-------------------\n" res
    opts