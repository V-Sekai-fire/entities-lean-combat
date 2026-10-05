# entities-lean-combat

A deterministic combat reducer and timed-combo state machine in Lean 4, with property tests and a golden-trace emitter.

## What it is for

The reducer threads a pure, all-integer state through ticks, attacks and spawns, so a replay is bit-exact: an enemy's spawn-invulnerability window, a three-stage timed combo and the enemy's death. Fixtures in `combat_demo` fail its build when the reducer regresses, Plausible checks its invariants over random event streams, and `combat_emit` writes an event and effect trace for another implementation to match.

## Build and run

```sh
lake build combat_demo
lake exe combat_emit
```

## Licence

MIT; see LICENSE.
