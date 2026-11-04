# Advent of Code 2023 (Unison)

These solutions live on Unison Share as [`@mdr/aoc-2023`](https://share.unison-lang.org/@mdr/aoc-2023). Unison keeps code in a codebase rather than in text files, so this directory holds a **text export** as a backup:

- [`aoc-2023.u`](aoc-2023.u): every definition in the project (days 1–21, `utils`, `up` and the README doc) as Unison source, without the libraries.
- [`aliases.md`](aliases.md): 10 second names for definitions, as UCM commands. A source file defines each definition under one name only.

## Libraries

| Namespace in the project | Share release |
|---|---|
| `lib.base` | `@unison/base/releases/2.9.1` |
| `lib.cloud_6_0_6` | `@unison/cloud/releases/6.0.6` |
| `lib.dataextra_0_1_5` | `@stew/dataextra/releases/0.1.5` |
| `lib.parsing_2_2_0` | `@rlmark/parsing/releases/2.2.0` |

`up.lib.base` isn't a library: it holds two of the project's own definitions, which are in the export.

## How faithful it is

Restored into an empty project (below), every one of the project's 1,237 names comes back with its original hash, except one test. `day16.solvePart1.tests.example1` refers to an earlier version of `day16.solvePart1` that no longer has a name, so UCM can only print it as a hash (`#l0vjqg2soc`), which doesn't parse. The export points that test at the current `day16.solvePart1` instead. All 291 tests pass.

Each `type` and `ability` declaration carries its unique ID (`unique[…] type …`): it's part of the type's hash, and without it a restore would give every type, and everything using one, a new hash.

## Updating the export

```sh
nix develop --command ./export.py
```

This clones `@mdr/aoc-2023/main` into a temporary codebase, writes out the source with UCM's `edit.namespace`, and checks the result by restoring it into an empty project and comparing every name and hash with the original. It takes several minutes.

## Restoring

If the project is still on Share, `clone @mdr/aoc-2023/main` in UCM is the easiest way. From this export, in UCM (`nix develop --command ucm`):

1. `project.create-empty aoc-2023`
2. Install the libraries from the table above, e.g. `lib.install @unison/base/releases/2.9.1`.
3. `load aoc-2023.u`, then `add`.
4. Run the commands in `aliases.md`.

`export.py` runs exactly these steps to check the export.
