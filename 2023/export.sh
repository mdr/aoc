#!/usr/bin/env bash
# Regenerates aoc-2023.u from @mdr/aoc-2023/main on Unison Share, then checks
# that the file loads back with every definition unchanged except the one
# patched below. Run inside `nix develop`.
set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

# Clone into a throwaway codebase (kept for the verification step).
cat > "$work/clone.md" <<'MD'
```ucm
scratch/main> clone @mdr/aoc-2023/main aoc-2023/main
```
MD
ucm transcript.in-place -C "$work/codebase" "$work/clone.md" > /dev/null

# Print every definition outside lib as source.
cat > "$work/export.md" <<'MD'
```ucm
aoc-2023/main> edit.namespace .
```
MD
ucm transcript.fork -c "$work/codebase" "$work/export.md" > /dev/null
awk '/^``` unison :added-by-ucm scratch.u$/ {f = 1; next} f && /^```$/ {f = 0} f' \
  "$work/export.output.md" > "$work/aoc-2023.u"

# This test refers to an earlier, no longer named version of day16.solvePart1,
# which UCM prints as a hash that can't be parsed. Point it at the current one.
sed -i.bak 's/check (#l0vjqg2soc day16.inputs.example1 === 46)/check (day16.solvePart1 day16.inputs.example1 === 46)/' \
  "$work/aoc-2023.u"

# Load the file back into the clone: only the patched test may differ.
cat > "$work/verify.md" <<MD
\`\`\`ucm
aoc-2023/main> load $work/aoc-2023.u
\`\`\`
MD
ucm transcript.fork -c "$work/codebase" "$work/verify.md" > /dev/null
changes=$(grep -E '^  [~+-] [^(]' "$work/verify.output.md" || true)
if [ "$changes" != "  ~ day16.solvePart1.tests.example1 : [Result]" ]; then
  echo "Unexpected changes when loading the export:" >&2
  echo "$changes" >&2
  exit 1
fi
if grep -qE 'FAILED|🚫' "$work/verify.output.md"; then
  echo "Some tests failed when loading the export" >&2
  exit 1
fi

cp "$work/aoc-2023.u" "$here/aoc-2023.u"
echo "Wrote $here/aoc-2023.u ($(grep -c '' "$here/aoc-2023.u") lines)"
