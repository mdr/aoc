Run after loading and adding aoc-2023.u, e.g. `ucm transcript.in-place -c <codebase> aliases.md`.
These are second names for definitions, which a Unison source file can't express.

```ucm
aoc-2023/main> alias.term day01.digitWords day01.forwardDigitWords
aoc-2023/main> alias.term day06.Race.distance day06.Race.recordDistance
aoc-2023/main> alias.term day06.Race.distance.modify day06.Race.recordDistance.modify
aoc-2023/main> alias.term day06.Race.distance.set day06.Race.recordDistance.set
aoc-2023/main> alias.term day06.Race.raceDuration day06.Race.time
aoc-2023/main> alias.term day06.Race.raceDuration.modify day06.Race.time.modify
aoc-2023/main> alias.term day06.Race.raceDuration.set day06.Race.time.set
aoc-2023/main> alias.term utils.findCycle.tests.case1 utils.findCycle.tests.example
aoc-2023/main> alias.term utils.findCycle.tests.case1 utils.findCycleFloyd.tests.example
aoc-2023/main> alias.term day04.parser.numberMaybeWhitespace utils.parser.numberMaybeWhitespace
```
