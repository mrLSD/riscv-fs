# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

`riscv-fs` is a formal RISC-V ISA simulator written in F#: it decodes and executes
RISC-V instructions, loads ELF files, and runs from a CLI.

## [0.6.0] - 2026-06-02

The **"C" (Compressed)** extension release. ([#20])

### Added

- **RV32C / RV64C "C" (Compressed) Standard Extension** — full integer subset, decode
  (`DecodeC.fs`) and execute (`ExecuteC.fs`) for every 16-bit instruction across all three
  quadrants: `C.ADDI4SPN`, `C.LW`/`C.SW`, `C.LD`/`C.SD` (RV64), `C.ADDI`/`C.NOP`,
  `C.JAL` (RV32), `C.ADDIW` (RV64), `C.LI`, `C.LUI`, `C.ADDI16SP`,
  `C.SRLI`/`C.SRAI`/`C.ANDI`, `C.SUB`/`C.XOR`/`C.OR`/`C.AND`, `C.SUBW`/`C.ADDW` (RV64),
  `C.J`, `C.BEQZ`/`C.BNEZ`, `C.SLLI`, `C.LWSP`/`C.LDSP` (RV64), `C.JR`/`C.JALR`,
  `C.MV`, `C.ADD`, `C.EBREAK`, `C.SWSP`/`C.SDSP` (RV64).
- Eight new architecture variants: `rv32ic`, `rv32imc`, `rv32iac`, `rv32imac`,
  `rv64ic`, `rv64imc`, `rv64iac`, `rv64imac` (`Arch.fromString`, `hasC` predicate).
- Automatic instruction-length detection from `inst[1:0]`, derived in `Decoder.Decode` and
  baked into the returned executor, so compressed (2-byte) and standard (4-byte)
  instructions advance the PC and link addresses correctly through any decode→execute path.
- `IALIGN = 2` when the C extension is present: 2-byte-aligned jump/branch targets are
  legal and no longer trap.
- HINT handling per the C-extension spec: `C.LUI`/`C.LI`/`C.MV`/`C.ADD`/`C.ADDI`/`C.SLLI`
  with `rd = x0` (and shamt-0 shifts) execute as no-ops instead of trapping.
- New test suites `Tests/rvc/c.fs` and `Tests/unit/branch.fs`; the project now reaches
  **100% line coverage and 100% branch coverage**.

### Changed

- `Decoder.Decode` now owns `InstrLen` (removing the hidden dependency on externally-set
  machine state for correct compressed-instruction PC/link advancement).
- CLI `--arch` help lists all 16 supported architectures and marks `-A/--arch` as
  **required** (it previously advertised a non-existent `rv32i` default).
- `MachineState.storeMemory*` refactored into a single recursive `storeBytes` helper;
  CLI argument handling uses `Array.skip` instead of array slicing (clearer and
  branch-complete).

### Fixed

- `C.LUI` with `rd = x0, nzimm ≠ 0` is decoded as a HINT (no-op) instead of an illegal
  instruction — direct RISC-V C-extension conformance fix.
- `C.ADDIW rd, 0` correctly behaves as `sext.w rd` (the `imm = 0` encoding is valid, not
  reserved); added an explicit regression test.
- Removed a dead defensive branch in the `Program.fs` error handler.

## [0.5.0] - 2026-06-01

Robustness, conformance, and tooling release: .NET 10, an RV32 shift fix, and a full
audit pass. ([#17], [#18], [#19])

### Added

- Run-loop **step limit**: a non-terminating program (e.g. a backward self-branch) aborts
  with a `StepLimit` trap instead of hanging. ([#19])
- ELF loader now loads **all `PT_LOAD` segments** (code, data, zero-filled `.bss`) for both
  32- and 64-bit ELF. ([#19])
- LR/SC **reservation semantics** (so `SC` can fail) and misaligned-atomic traps. ([#19])
- Verbose **instruction trace** (`-v`) and dynamic CLI version/author (read from assembly
  metadata, dynamic year). ([#19])
- **Code coverage in CI** (Codecov) and expanded unit/integration tests for CLI, decoding,
  execution, atomics, and ELF handling. ([#19])

### Changed

- Upgraded to **.NET 10** (from .NET 8). ([#17])
- ELF loading and the run cycle are wrapped in exception handling — malformed input is
  reported instead of crashing. ([#19])
- Project version is single-sourced from `<Version>` in the `.fsproj` and read via assembly
  metadata. ([#19])
- `setRegister` is now immutable (copies the register array, preserving prior states). ([#19])
- CI migrated to GitHub Actions. ([#19])

### Fixed

- RV32 `SLL`/`SRL`/`SRA` now mask the shift amount to `rs2[4:0]`; shifts ≥ 32 previously
  produced wrong results. ([#18])
- RV64 `IMA` opcode conformance: `.D` atomics funct5/`rs2` decoding, RV64 6-bit shamt
  (`inst[25:20]`), and `DIVUW`/`REMUW` operating on the low 32 bits (unsigned). ([#19])
- 32-bit `AMO.W` uses the correct `rs2` width. ([#19])
- RV32 `DIV`/`REM` overflow edge case. ([#19])
- CLI long-key value argument-index handling. ([#19])
- Hygiene: module rename, typo fixes, `StartsWith` usage, and documentation comments. ([#19])

## [0.4.1] - 2024-01-27

Maintenance release. ([#12], [#14], [#15])

### Added

- Continuous Integration pipeline. ([#15])

### Changed

- Updated to **.NET 8** and **F# 5**. ([#12])
- Cleaned up `Bits.fs` helper functions. ([#14])

### Fixed

- `ExecuteI` instruction-execution fixes. ([#15])

## [0.4.0] - 2020-06-11

The **"A" (Atomic)** extension release. ([#10], [#11])

### Added

- **RV32A / RV64A "A" (Atomic Memory Operations) Standard Extension**: `LR`/`SC` and the
  AMO operations (`SWAP`, `ADD`, `XOR`, `AND`, `OR`, `MIN`, `MAX`, `MINU`, `MAXU`) for both
  `.W` and `.D`. ([#10])
- `MachineState` store-to-memory methods. ([#10])
- Comprehensive AMO test suite with assembler sources. ([#11])

### Changed

- `SB`/`SH`/`SW`/`SD` store instructions reworked to use the `MachineState` store-to-memory
  methods. ([#10])
- CLI options reworked. ([#10])

## [0.3.0] - 2019-12-09

The **"M" (Multiply/Divide)** extension release. ([#8], [#9])

### Added

- **RV32M / RV64M "M" (Integer Multiplication and Division) Standard Extension**:
  `MUL`, `MULH`, `MULHSU`, `MULHU`, `DIV`, `DIVU`, `REM`, `REMU` (and the 64-bit `*W`
  variants). ([#8])
- M-extension test suite (32- and 64-bit) with assembler sources. ([#9])

### Fixed

- `MULHU` algorithm. ([#8])

## [0.2.0] - 2019-11-09

The **RV64I** base release. ([#6], [#7])

### Added

- **RV64I** base integer instruction set: 64-bit decoder and executor, the `*W`
  word instructions, and 64-bit `shamt` handling. ([#6])
- RV64I test suite — ALU, ALU-immediate, branches, jumps, upper-immediate, system, and
  memory tests. ([#7])

### Changed

- Instructions widened to a 32-bit field representation and all dependencies updated. ([#6])
- Decoder logic reworked to support both RV32 and RV64. ([#6])

## [0.1.0] - 2019-10-26

Initial release: the **RV32I** base integer ISA. ([#1], [#2], [#3], [#4], [#5])

### Added

- **RV32I** base integer instruction set — decoder and executor for `LUI`, `AUIPC`, `JAL`,
  `JALR`, the branch family, loads/stores, the ALU and ALU-immediate families, `FENCE`,
  `ECALL`, and `EBREAK`.
- ELF file reading and a CLI with verbosity output.
- Comprehensive RV32I test suite (ALU, ALU-immediate, branches, jumps, upper-immediate,
  memory, system) with assembler test sources. ([#4])
- TravisCI integration. ([#1])
- Project documentation: README, `CONTRIBUTING.md`, `CODE_OF_CONDUCT.md`, and issue
  templates. ([#2], [#5])

### Changed

- Opcode representation reworked for the decode flow. ([#3])

### Fixed

- Branch instructions, `BLTU`/`BGEU`, `SRL`, `SRLI`/`SLTIU`, and `JALR` execution.

[0.6.0]: https://github.com/mrLSD/riscv-fs/compare/v0.5.0...v0.6.0
[0.5.0]: https://github.com/mrLSD/riscv-fs/compare/v0.4.1...v0.5.0
[0.4.1]: https://github.com/mrLSD/riscv-fs/compare/v0.4.0...v0.4.1
[0.4.0]: https://github.com/mrLSD/riscv-fs/compare/v0.3.0...v0.4.0
[0.3.0]: https://github.com/mrLSD/riscv-fs/compare/v0.2.0...v0.3.0
[0.2.0]: https://github.com/mrLSD/riscv-fs/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/mrLSD/riscv-fs/releases/tag/v0.1.0

[#1]: https://github.com/mrLSD/riscv-fs/pull/1
[#2]: https://github.com/mrLSD/riscv-fs/pull/2
[#3]: https://github.com/mrLSD/riscv-fs/pull/3
[#4]: https://github.com/mrLSD/riscv-fs/pull/4
[#5]: https://github.com/mrLSD/riscv-fs/pull/5
[#6]: https://github.com/mrLSD/riscv-fs/pull/6
[#7]: https://github.com/mrLSD/riscv-fs/pull/7
[#8]: https://github.com/mrLSD/riscv-fs/pull/8
[#9]: https://github.com/mrLSD/riscv-fs/pull/9
[#10]: https://github.com/mrLSD/riscv-fs/pull/10
[#11]: https://github.com/mrLSD/riscv-fs/pull/11
[#12]: https://github.com/mrLSD/riscv-fs/pull/12
[#14]: https://github.com/mrLSD/riscv-fs/pull/14
[#15]: https://github.com/mrLSD/riscv-fs/pull/15
[#17]: https://github.com/mrLSD/riscv-fs/pull/17
[#18]: https://github.com/mrLSD/riscv-fs/pull/18
[#19]: https://github.com/mrLSD/riscv-fs/pull/19
[#20]: https://github.com/mrLSD/riscv-fs/pull/20
