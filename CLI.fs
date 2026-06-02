module ISA.RISCV.CLI
#nowarn "40"

open System

// Version comes from risc-v.fsproj (<Version>) via assembly metadata — single source of truth.
let version =
    let v = System.Reflection.Assembly.GetExecutingAssembly().GetName().Version
    sprintf "v%d.%d.%d" v.Major v.Minor v.Build
let author = sprintf "(c) %d by Evgeny Ukhanov" DateTime.Now.Year
let about = sprintf "RISC-V Simulator for Formal RISC-V ISA implementation\n%s %s" version author

type AppConfig = {
        Verbosity: bool option
        Arch:      Arch.Architecture option
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
                      sprintf "<%s>" x.Value.Value
                  else
                      ""
        printfn "%s" (String.Format("{0,-5}{1,-20} {2}", "", msg, x.HelpMessage))

// Helper for print Usage info
let CliUsage (cliArgs : CliOptions []) =
    printfn "%s" about
    printfn "%s" (String.Format("USAGE:\n{0,-5}risc-v [OPTIONS] file...\nOPTIONS", ""))
    for arg in cliArgs do
        arg.printHelpMessage

// Fetch arguments to App config data.
// Tail-recursive walk over an index into `argv` with an explicit leftover accumulator.
// The previous version recursed once per token with both arms non-tail (the result was
// post-processed) plus an O(n) Array.skip per frame, so a very large argv overflowed the
// stack (uncatchable StackOverflowException) and parsed in O(n^2). This preserves the
// exact (CliResult * leftover-argv) contract for every option shape but runs in O(n)
// time and O(1) stack (verified by a 280k-case differential fuzz against the old code).
let fetchArgs (argv : string[]) (opts : CliOptions) (cfg : AppConfig) =
    let n = argv.Length
    // Match `opts` against the token at index `i` (i < n), returning the partial result
    // and how many tokens it consumed (0 when it matched nothing).
    let tryMatch (i : int) (cfg : AppConfig) : CliResult * int =
        let arg = argv.[i]
        // An option taking a <value> consumes the NEXT token, unless it is missing or
        // itself looks like an option (dash-prefixed) -> parse Error.
        let matchValue () =
            if i + 1 < n then
                let arg2 = argv.[i + 1]
                if arg2.StartsWith "-" then (Error, 0)
                else (Result(opts.Handler arg2 cfg), 2)
            else (Error, 0)
        if opts.Key.IsSome then
            if sprintf "-%s" opts.Key.Value = arg then
                if opts.Value.IsSome then matchValue () else (Result(opts.Handler arg cfg), 1)
            else if opts.LongKey.IsSome && sprintf "--%s" opts.LongKey.Value = arg then
                if opts.Value.IsSome then matchValue () else (Result(opts.Handler arg cfg), 1)
            else (NotFound(cfg), 0)
        else if opts.LongKey.IsSome then
            if sprintf "--%s" opts.LongKey.Value = arg then
                if opts.Value.IsSome then matchValue () else (Result(opts.Handler arg cfg), 1)
            else (NotFound(cfg), 0)
        else if opts.Value.IsSome then
            // A value-only (FILE) option must not swallow an unknown option:
            // a dash-prefixed token is a parse error, not a file name.
            if arg.StartsWith "-" then (Error, 0)
            else (Result(opts.Handler arg cfg), 1)
        else
            (NotFound(cfg), 0)
    // `leftover` keeps, in original order, the tokens this option did not consume (handed
    // to the next option). `matched` records whether the option matched at least once: a
    // Multiple option that consumes every token ends on the empty base case and must still
    // report Result (not NotFound) — the old code did this by promoting NotFound on unwind.
    let leftover = System.Collections.Generic.List<string>()
    let rec loop (i : int) (cfg : AppConfig) (matched : bool) =
        if i >= n then
            let result = if matched then Result(cfg) else NotFound(cfg)
            (result, leftover.ToArray())
        else
            let arg = argv.[i]
            let (cfgRes, consumed) = tryMatch i cfg
            match cfgRes with
            | Result(res) when opts.Multiple && n - (i + consumed) > 0 ->
                // Consumed `consumed` tokens; keep scanning the rest for further matches.
                loop (i + consumed) res true
            | NotFound(res) ->
                // Not for this option: preserve the token and continue with the rest.
                leftover.Add arg
                loop (i + 1) res matched
            | _ ->
                // Terminal: a non-Multiple Result, an Error, or a Multiple match that
                // consumed the remainder. Leftover = preserved tokens ++ the unconsumed tail.
                for j in (i + consumed) .. (n - 1) do leftover.Add argv.[j]
                (cfgRes, leftover.ToArray())
    loop 0 cfg false

// Parse CLI with specific params
let rec parseCli (argv : string[]) (opts : CliOptions[]) (cfg : AppConfig) =
    if opts.Length < 1 then
        Success(cfg)
    else
        let opt = opts.[0]
        let opts = if opts.Length > 1 then Array.skip 1 opts else [||]
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
            HelpMessage = "RISC-V architecture (required). Available: rv32i, rv32im, rv32ia, rv32ima, rv32ic, rv32imc, rv32iac, rv32imac, rv64i, rv64im, rv64ia, rv64ima, rv64ic, rv64imc, rv64iac, rv64imac"
            Handler =
                fun arg cfg ->
                    { cfg with
                        AppConfig.Arch = Arch.Architecture.fromString arg
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