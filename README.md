# RISC-V formal ISA Specification

Copyright &copy; Evgeny Ukhanov

This is a formal (and executable) specification for the 
RISC-V ISA (Instruction Set Architecture), written in 
**F# purely functional style**. We deliberately choose 
an "_extremely elementary_" implementation of F# to make it 
readable and usable by wide audience who do not know F# and who 
do not plan to learn F#.

This is a work-in-progress, one of several similar concurrent 
efforts within the **ISA Formal Specification** 
Technical Group constituted by The RISC-V Foundation 
(https://riscv.org). We welcome your feedback, comments and suggestions. 

## Content
* Features & Current status 

## Features & Current status
* Supports the following features
  * Base instruction sets: RV32I
* Features under development
  * Base instruction sets: RV64I
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
  * rv32ui-p-*, rv64ui-p-* (Base instruction set)
  * rv32um-p-*, rv64um-p-* (M extension)
  * rv32ua-p-*, rv64ua-p-* (A extension)
  * rv32uc-p-*, rv64uc-p-* (C extension)

## Reading the code
We expect that many people might use this as a reading reference (whether or not they build and execute it) to clarify their understanding of RISC-V ISA semantics.

## Licence
**MIT License**