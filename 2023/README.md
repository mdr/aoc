# Advent of Code 2023 (Unison)

These solutions live on Unison Share as [`@mdr/aoc-2023`](https://share.unison-lang.org/@mdr/aoc-2023). Unison keeps code in a codebase rather than in text files, so this directory holds a **text export** as a backup: [`aoc-2023.u`](aoc-2023.u) has every definition in the project (days 1–21, `utils`, `up` and the README doc), but not its libraries.

## Libraries

| Namespace in the project | Share project |
|---|---|
| `lib.base` | `@unison/base` (the codebase doesn't record which release) |
| `lib.cloud_6_0_6` | `@unison/cloud/releases/6.0.6` |
| `lib.dataextra_0_1_5` | `@stew/dataextra/releases/0.1.5` |
| `lib.parsing_2_2_0` | `@rlmark/parsing/releases/2.2.0` |

## How faithful it is

Loaded into a clone of the project, the export reproduces all 61 types and 1,065 terms with identical hashes, except one test. `day16.solvePart1.tests.example1` refers to an earlier version of `day16.solvePart1` that no longer has a name, so UCM can only print it as a hash (`#l0vjqg2soc`), which doesn't parse. The export points that test at the current `day16.solvePart1` instead. All 291 tests pass.

## Updating the export

```sh
nix develop --command ./export.sh
```

This clones `@mdr/aoc-2023/main` into a temporary codebase, writes out the source with UCM's `edit.namespace .`, and loads the result back into the clone to check that nothing changed apart from the test above.

## Restoring

If the project is still on Share, `clone @mdr/aoc-2023/main` in UCM is the easiest way. From this export (these steps haven't been tested end to end):

1. In UCM (`nix develop --command ucm`), create a project: `project.create-empty aoc-2023`.
2. Install the libraries from the table above, e.g. `lib.install @unison/cloud/releases/6.0.6`.
3. `load aoc-2023.u`, then `add`.
