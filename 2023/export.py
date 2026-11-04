#!/usr/bin/env python3
"""Export the Unison project @mdr/aoc-2023 to text, and check the export.

Writes aoc-2023.u (every definition, as Unison source) and aliases.md (UCM
commands that recreate names the source can't express), then checks them:

1. Loaded into a clone of the project, nothing changes except PATCHED_TEST.
2. Restored into an empty project, with the LIBRARIES releases, the export and
   then aliases.md, every name has the same hash as in the original, again
   except PATCHED_TEST.

Run inside `nix develop`: ./export.py
"""

import re
import shutil
import subprocess
import sys
import tempfile
from collections import defaultdict
from pathlib import Path

SHARE_PROJECT = "@mdr/aoc-2023/main"
BRANCH = "aoc-2023/main"
LIBRARIES = [
    "@unison/base/releases/2.9.1",
    "@unison/cloud/releases/6.0.6",
    "@stew/dataextra/releases/0.1.5",
    "@rlmark/parsing/releases/2.2.0",
]
# edit.namespace skips namespaces called lib, which includes up.lib. That holds
# two of the project's own definitions, not a library, so name it explicitly.
EXPORT_NAMESPACES = [".", "up.lib"]
# This test refers to an earlier, no longer named version of day16.solvePart1,
# which UCM prints as a hash that can't be parsed. Point it at the current one.
PATCHED_TEST = "day16.solvePart1.tests.example1"
PATCH = (
    "check (#l0vjqg2soc day16.inputs.example1 === 46)",
    "check (day16.solvePart1 day16.inputs.example1 === 46)",
)

HERE = Path(__file__).resolve().parent


class ExportError(Exception):
    pass


class Ucm:
    """Runs UCM transcripts in a working directory."""

    def __init__(self, work: Path):
        self.work = work

    def run(self, name: str, codebase: Path, commands: list[str], fork: bool = True) -> str:
        """Runs commands as a transcript and returns its output."""
        transcript = self.work / f"{name}.md"
        transcript.write_text("```ucm\n" + "\n".join(commands) + "\n```\n")
        mode = ["transcript.fork", "-c"] if fork else ["transcript.in-place", "-C"]
        result = subprocess.run(["ucm", *mode, str(codebase), str(transcript)], capture_output=True, text=True)
        output = self.work / f"{name}.output.md"
        if result.returncode != 0:
            raise ExportError(f"ucm failed on {transcript}; see {output}")
        return output.read_text()


def scratch_block(output: str) -> str:
    """The Unison source that edit/edit.namespace adds to a transcript's output."""
    match = re.search(r"^``` unison :added-by-ucm scratch\.u\n(.*?)^```$", output, re.M | re.S)
    if not match:
        raise ExportError("No source in UCM's output")
    return match.group(1)


def names_and_hashes(ucm: Ucm, codebase: Path, label: str) -> dict[str, str]:
    """Every name in the branch (outside lib), mapped to its hash."""
    output = ucm.run(f"find-{label}", codebase, [f"{BRANCH}> find.verbose"])
    names = {}
    # Entries look like:   12.   -- #hash
    #                            name1,
    #                            name2 : Type
    for entry in re.split(r"\n\s+\d+\.\s+-- ", output)[1:]:
        hash_, rest = entry.split("\n", 1)
        for name in re.split(r",\s*", rest.split(" : ", 1)[0].strip()):
            names[name.strip()] = hash_.strip()
    return names


def export_source(ucm: Ucm, codebase: Path, label: str) -> str:
    output = ucm.run(label, codebase, [f"{BRANCH}> edit.namespace {' '.join(EXPORT_NAMESPACES)}"])
    return scratch_block(output)


def defines(source: str, name: str) -> bool:
    """Whether the source defines name, directly or as a record accessor."""
    if re.search(rf"^(?:test> )?\(?{re.escape(name)}\)? *[:=]", source, re.M):
        return True
    parts = name.split(".")
    if parts[-1] in ("set", "modify"):
        parts = parts[:-1]
    type_name, field = ".".join(parts[:-1]), parts[-1]
    record = re.search(rf"^type {re.escape(type_name)}\n  = \{{([^}}]*)\}}", source, re.M)
    return bool(record and re.search(rf"\b{re.escape(field)} :", record.group(1)))


def plan_aliases(names: dict[str, str], source: str) -> list[tuple[str, str]]:
    """(kept, alias) pairs for hashes with several names.

    The export prints each definition once, under one name, and code may use
    another of its names, which the file then doesn't define. So all but the
    name the export defines are deleted before exporting and recreated after
    restoring.
    """
    by_hash = defaultdict(list)
    for name, hash_ in names.items():
        by_hash[hash_].append(name)
    pairs = []
    for hash_, group in by_hash.items():
        if len(group) < 2:
            continue
        kept = [name for name in group if defines(source, name)]
        if len(kept) != 1:
            raise ExportError(f"Can't tell which name the export defines for {hash_}: {group}")
        pairs += [(kept[0], alias) for alias in group if alias != kept[0]]
    return sorted(pairs, key=lambda pair: pair[1])


def add_unique_ids(ucm: Ucm, codebase: Path, source: str) -> str:
    """Writes each type's unique ID into its declaration.

    A plain `type` or `ability` gets a random ID when it's added to a codebase,
    and the ID is part of its hash. Without it, a restore gives every type, and
    everything that uses one, a new hash.
    """
    declarations = re.findall(r"^(type|ability) ([^\s]+)", source, re.M)
    output = ucm.run("unique-ids", codebase, [f"{BRANCH}> debug.type.abt {name}" for _, name in declarations])
    sections = re.split(rf"^{re.escape(BRANCH)}> debug\.type\.abt ", output, flags=re.M)[1:]
    for (kind, name), section in zip(declarations, sections, strict=True):
        match = re.search(r'modifier = Unique "([^"]+)"', section)
        if not section.startswith(name + "\n") or not match:
            raise ExportError(f"No unique ID found for {name}")
        source, count = re.subn(rf"^{kind} {re.escape(name)}(?=\s)", f"unique[{match.group(1)}] {kind} {name}", source, flags=re.M)
        if count != 1:
            raise ExportError(f"Expected one declaration of {name}, found {count}")
    return source


def export(ucm: Ucm, work: Path) -> tuple[str, list[tuple[str, str]], int]:
    codebase = work / "codebase"
    ucm.run("clone", codebase, [f"scratch/main> clone {SHARE_PROJECT} {BRANCH}"], fork=False)
    original = names_and_hashes(ucm, codebase, "original")

    aliases = plan_aliases(original, export_source(ucm, codebase, "first-export"))
    if aliases:
        ucm.run("delete-aliases", codebase, [f"{BRANCH}> delete.term {alias}" for _, alias in aliases], fork=False)
    source = export_source(ucm, codebase, "export")
    source = add_unique_ids(ucm, codebase, source)
    if source.count(PATCH[0]) != 1:
        raise ExportError(f"Expected {PATCHED_TEST} to contain: {PATCH[0]}")
    source = source.replace(*PATCH)
    export_file = work / "aoc-2023.u"
    export_file.write_text(source)

    # Check 1: load into the clone; only the patched test may change.
    output = ucm.run("check-clone", codebase, [f"{BRANCH}> load {export_file}"])
    changes = re.findall(r"^  [~+-] ([^(\s].*)$", output, re.M)
    if changes != [f"{PATCHED_TEST} : [Result]"]:
        raise ExportError(f"Unexpected changes loading into the clone: {changes}")

    # Check 2: restore into an empty project; every name must have its
    # original hash, except the patched test.
    restored = work / "restored"
    ucm.run(
        "restore",
        restored,
        [f"scratch/main> project.create-empty {BRANCH.split('/')[0]}"]
        + [f"{BRANCH}> lib.install {library}" for library in LIBRARIES]
        + [f"{BRANCH}> load {export_file}", f"{BRANCH}> add"]
        + [f"{BRANCH}> alias.term {kept} {alias}" for kept, alias in aliases],
        fork=False,
    )
    restored_names = names_and_hashes(ucm, restored, "restored")
    differences = sorted(
        name for name in original.keys() | restored_names.keys() if original.get(name) != restored_names.get(name)
    )
    if differences != [PATCHED_TEST]:
        raise ExportError(f"Restored project differs from the original: {differences}")
    return source, aliases, len(original)


def main() -> None:
    work = Path(tempfile.mkdtemp())
    try:
        source, aliases, name_count = export(Ucm(work), work)
    except ExportError as error:
        sys.exit(f"{error}\n(working files kept in {work})")
    (HERE / "aoc-2023.u").write_text(source)
    (HERE / "aliases.md").write_text(
        "Run after loading and adding aoc-2023.u, e.g. `ucm transcript.in-place -c <codebase> aliases.md`.\n"
        "These are second names for definitions, which a Unison source file can't express.\n\n"
        "```ucm\n" + "".join(f"{BRANCH}> alias.term {kept} {alias}\n" for kept, alias in aliases) + "```\n"
    )
    shutil.rmtree(work)
    print(f"Wrote aoc-2023.u ({source.count(chr(10))} lines) and aliases.md ({len(aliases)} aliases).")
    print(f"A restore reproduces all {name_count} names with their original hashes, except {PATCHED_TEST}.")


if __name__ == "__main__":
    main()
