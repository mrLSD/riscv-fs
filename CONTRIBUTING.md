# Contributing to F# RISC-V ISA Formal Specification

We are welcomes contribution from everyone in the form of [suggestions], [bug
reports] or pull requests. This document gives some guidance if you are
thinking of helping us.

[README] can help you to better understand current
state of the project.

You can always ask for help or guidance.

## Quick Start

Install .NET Core and mandatory dependencies according to our [Readme installation guide],
then you can build the project and run the tests:

```shell
git clone git@github.com:mrLSD/riscv-fs.git
cd riscv-fs
dotnet test
```

## Finding something to fix or improve

You can prepare Pull Request.

## Linters

Currently we don't use linters.

## Conventions

Generally, we follow common best practices established in the F# functional programming.

Additional Notes:

- Create as minimal pull request as possible: they are easier to review and integrate.
- Write detailed description for Pull Request as possible as you can. It help us to integrate it.
- Use latest .NET Core version.

[suggestions]: https://github.com/mrLSD/riscv-fs/issues/new?assignees=mrLSD&labels=&template=feature_request.md&title=RISC-V+formal+specification+feature+request%3A
[bug reports]: https://github.com/mrLSD/riscv-fs/issues/new?assignees=mrLSD&labels=&template=bug_report.md&title=F%23+RISC-V+issue%3A
[README]: README.md
