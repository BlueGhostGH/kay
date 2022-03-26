# Kay

A low-level general-purpose imperative programming language inspired by Rust and Zig.

## Goals

Kay is a toy programming language and as of right now is a hobby project with no consistency in the amount of work getting done, as I work on it whenever I can and feel like it. As of now it has a very basic parser, with the current goal being implementing a very barebones static analysis. Once static analysis is in a working state, codegen to C will come next.

## Commands

Parse a `.kay` file:

```sh
cargo run -- [PATH]
```

Run parser tests:

```sh
cargo test
```
