# RISC-V formal ISA Specification
[![Build Status](https://travis-ci.org/mrLSD/riscv-fs.svg?branch=master)](https://travis-ci.org/mrLSD/riscv-fs)

**Copyright &copy; Evgeny Ukhanov**

This is a formal (and executable) specification for the 
RISC-V ISA (Instruction Set Architecture), written in 
**F# purely functional style**. We deliberately choose 
an "_extremely elementary_" implementation of F# to make it 
readable and usable by wide audience who do not know F# and who 
do not plan to learn F#.

![F# RISC-V ISA Formal Specification](https://miro.medium.com/max/2474/1*88Zj-QJq48IZTiCGUo5mSQ.png)

This is a work-in-progress, one of several similar concurrent 
efforts within the **ISA Formal Specification** 
Technical Group constituted by The RISC-V Foundation 
(https://riscv.org). We welcome your feedback, comments and suggestions. 

## Content
* [Features & Current status](#features--current-status) 
* [Reading the code](#reading-the-code)
* [How to build and run it on RISC-V binaries](#how-to-build-and-run-it-on-risc-v-binaries)
  * [Install .NET SDK](#install-.net-sdk)
  * [Make the application executable](#make-the-application-executable)
  * [Run the application executable](#run-the-application-executable)
* [References](#references)  
* [Licence](#licence)
 
## Features & Current status
* Supports the following features (or _in active development state_)
  - [x] Base instruction sets: RV32I
  - [x] Tests RV32I
  - [ ] Base instruction sets: RV64I
  - [ ] Tests RV64I
* Features under development
  * Standard extension M (integer multiply/divide)
  * Standard extension A (atomic memory ops)
  * Standard extension C (Compressed 16-bit instructions)
  * Standard extension F (Single-precision floating point)
  * Standard extension D (Double-precision floating point)
  * Privilege Level M (Machine)
  * Privilege Level U (User)
  * Privilege Level S (Supervisor)
    * Virtual Memory schemes SV32, SV39 and SV48
* Application can be executed as a F# program flexible with 
CLI (_command line interface_) support, which in 
turn executes RISC-V ELF binaries. This is a sequential 
interpretation: one-instruction-at-a-time, sequential 
memory model.
* Tests passing for RISC-V **under development**:
  * Basic instruction flow
  * `rv32ui-p-*, rv64ui-p-*` (Base instruction set)
  * `rv32um-p-*, rv64um-p-*` (M extension)
  * `rv32ua-p-*, rv64ua-p-*` (A extension)
  * `rv32uc-p-*, rv64uc-p-*` (C extension)

## Reading the code
We expect that many people might use this as a reading 
reference (whether or not they build and execute it) to 
clarify their understanding of RISC-V ISA semantics.

Main part for reading Specification:
* **Decode\*.fs**
  
  Decodes contain decoders for specific instructions set
  and notified with instruction/extension set symbol. For example `DecodeI.fs`
* **Execute\*.fs**

  Executes contain executions for specific instructions set
  and notified with instruction/extension set symbol. For example `ExecuteI.fs`
  
* Utilities:
  * `CLI.fs`
    
    Contain helper function and types for
    building effective CLI commands and options.
  
  * `Bits.fs`
    
    Basic type specific functions for 
    manipulations with `bits`.
  
  * `Run.fs`
  
    Basic Run flow - fetch, decode, execute,
    logging execution flow.  

* Architecture
  * `Arch.fs`

    Basic architecture types for RISC-V specification.
  
  * `MachineState.fs`
  
    Basic type and functions described
    RISC-V machine state.  

* Main app
  * `Program.fs`
  
  Main application to execute **RISC-V simulator/emulator**.
  
* Test
  * `Test/*.fs`
  
    Contain unit-tests for instructions set
    and extensions
    
  * Test/asm/
  
    Contain Assembler test programs for
    manual testing RISC-V CPI implementation.
    It depend on **risc-v toolchain** and 
    it has special auto-build `Makefile`.      

## How to build and run it on RISC-V binaries
Application can be executed as a _sequential RISC-V simulator_ 
(sequential, one-instruction-at-a-time semantics), by 
building and executing it as a standard F# program.

Supported OS:
* Linux
* Windows
* MacOS

Supported **.NET SDK**:
* .NET SDK 2.2
* .NET SDK 3.0

### Install .NET SDK

For Windows preferred way to use Visual Studio.

Other examples will be for Linux.
Please follow to instruction https://dotnet.microsoft.com/download

For Ubuntu:
```
$ wget -q https://packages.microsoft.com/config/ubuntu/16.04/packages-microsoft-prod.deb -O packages-microsoft-prod.deb
$ sudo dpkg -i packages-microsoft-prod.deb
$ sudo apt-get update
$ sudo apt-get install apt-transport-https
$ sudo apt-get update
$ sudo apt-get install dotnet-sdk-3.0
```
To check installation:

`$ dotnet --version`

will tell you what version of `dotnet` you have.

### Make the application executable
You can build the application executable with:

`$ dotnet build`

### Run the application executable

Most simple way to run immediately `run` (without 
additional `build` command) to see command-line 
options on the executable:

`$ dotnet run -- --help`

If you run the application without option:

`$ dotnet run`

you'll receive error message:

> Wrong parameters put --help to get more information

**Example** to run specific ISA with extensions, verbosity
output and ELF file for execution in RISC-V CPI simulator:

`$ dotnet run -- -A rv32i -v myapp.elf`

## References

* github ISA manual: https://github.com/riscv/riscv-isa-manual
* RISC-V specification: https://riscv.org/specifications/
* RISC-V Formal Verification Framework: https://github.com/SymbioticEDA/riscv-formal

## Licence
**MIT License**
