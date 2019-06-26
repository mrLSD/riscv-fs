module ISA.RISCV.CLI

open System

type CliOptions = {
        Key:         string option
        LongKey:     string option
        Value:       string option
        Multiple:    bool
        HelpMessage: string
        Required:    bool
    } with
    static member Default =
        {
            Key =  None
            LongKey = None
            Value = None
            Multiple = false
            HelpMessage = ""
            Required = false
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
    let version = "v0.1.0"
    let author = "(c) 20019 by Evgeny Ukhanov"
    printfn "RISC-V Simulator for Formal RISC-V ISA implementation"
    printfn "%s %s" version author
    printfn "%s" (String.Format("USAGE:\n{0,-5}risc-v [OPTIONS] file...\nOPTIONS", ""))
    for arg in cliArgs do
        arg.printHelpMessage

let InitCLI =
    let opts = [|
        { CliOptions.Default with
            Key =  Some("A");
            LongKey = Some("arch");
            HelpMessage = "RISC-V architecture. Available: rv32i. Default: rv32i"
        };
        { CliOptions.Default with
            Key =  Some("v");
            HelpMessage = "Verbosity output"
        };
        { CliOptions.Default with
            Key =  Some("h");
            LongKey = Some("help");
            HelpMessage = "Print help message"
        };
        { CliOptions.Default with
            Key =  Some("V");
            LongKey = Some("version");
            HelpMessage = "Application version"
        };
    |]
    opts
