module ISA.RISCV.CLI

open System

let version = "v0.1.0"
let author = "(c) 20019 by Evgeny Ukhanov"
let about = sprintf "RISC-V Simulator for Formal RISC-V ISA implementation\n%s %s" version author

type AppConfig = {
        Verbosity: bool option
        Arch:      string
        Files:     string[]
    } with
    static member Default =
        {
            Verbosity = None
            Arch      = ""
            Files     = [||]
        }

type CliResult =
    | Result of AppConfig
    | Error
    | NotFound

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

let InitCLI (argv : string[]) =
    let opts = [|
        { CliOptions.Default with
            Key =  Some("A");
            LongKey = Some("arch");
            HelpMessage = "RISC-V architecture. Available: rv32i. Default: rv32i"
            Handler =
                fun arg cfg ->
                { cfg with
                    AppConfig.Arch = arg
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
    printfn "Args: %A" argv

    for a in argv do
        for cl in opts do
            if cl.Key.IsSome && sprintf "-%s" cl.Key.Value = a then
                let cfg = cl.Handler cl.Key.Value appCfg
                printfn "Key: %s | %A" cl.Key.Value cfg
            else if cl.LongKey.IsSome && sprintf "-%s" cl.Key.Value = a then
                let cfg = cl.Handler cl.LongKey.Value appCfg
                printfn "Key: %s | %A" cl.LongKey.Value cfg


    opts

let rec fetchArgs (argv : string[]) cl cfg =
    if argv.Length < 1 then
        None
    else
        let arg = argv.[0]
        let res =
            if cl.Key.IsSome then
                if  sprintf "-%s" cl.Key.Value = arg then
                    if cl.Value.IsSome then
                        if argv.Length < 2 then
                            let arg2 = argv.[1]
                            // Check is value not argument parameter
                            if "-" = arg2.[..0] then
                                (Error, 0)
                            else
                                let cfg = cl.Handler arg2 cfg
                                (Result(cfg), 2)
                        else
                            (Error, 0)
                    else
                        let cfg = cl.Handler arg cfg
                        printfn "Key: %s | %A" cl.Key.Value cfg
                        (Result(cfg), 1)
                else if cl.LongKey.IsSome && sprintf "-%s" cl.Key.Value = arg then
                    if cl.Value.IsSome then
                        if argv.Length < 2 then
                            let arg2 = argv.[1]
                            if "-" = arg2.[..0] then
                                (Error, 0)
                            else
                                let cfg = cl.Handler arg2 cfg
                                printfn "LongKey: %s | %A" cl.LongKey.Value cfg
                                (Result(cfg), 2)
                        else
                            (Error, 0)
                    else
                        let cfg = cl.Handler arg cfg
                        printfn "LongKey: %s | %A" cl.LongKey.Value cfg
                        (Result(cfg), 1)
                else
                    (NotFound, 0)

            else if cl.LongKey.IsSome then
                if sprintf "-%s" cl.LongKey.Value = arg then
                    if cl.Value.IsSome then
                        if argv.Length < 2 then
                            let arg2 = argv.[1]
                            if "-" = arg2.[..0] then
                                (Error, 0)
                            else
                                let cfg = cl.Handler arg2 cfg
                                printfn "LongKey: %s | %A" cl.LongKey.Value cfg
                                (Result(cfg), 2)
                        else
                            (Error, 0)
                    else
                        let cfg = cl.Handler arg cfg
                        printfn "LongKey: %s | %A" cl.LongKey.Value cfg
                        (Result(cfg), 1)
                else
                    (NotFound, 0)

            else if cl.Value.IsSome then
                let cfg = cl.Handler arg cfg
                printfn "Value: %s | %A" cl.Value.Value cfg
                (Result(cfg), 1)
            else
                (NotFound, 0)

        let (cfgRes, resIndex) = res
        match cfgRes with
        | Result(res) when cl.Multiple && argv.Length - resIndex > 0 -> fetchArgs argv.[resIndex..] cl res
        | NotFound when argv.Length - resIndex > 0 -> fetchArgs argv.[resIndex..] cl cfg
        | _ -> Some(cfg)

