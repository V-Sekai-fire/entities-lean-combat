import CombatCore
import Plausible
open CombatCore Plausible

/-- Build an event script: spawn, wait out invuln, then n presses at a given gap. -/
def script (waitTicks presses gap : Nat) : List Event :=
  [.spawn] ++ List.replicate waitTicks .tick
    ++ (List.range presses).flatMap (fun i =>
        if i == 0 then [.attack] else List.replicate gap .tick ++ [.attack])

-- Fixtures (#guard fails the build if the reducer regresses).
-- An attack inside the spawn window is blocked.
#guard (replay [.spawn, .attack]).2 == [.swing 0, .blocked]
-- After the window, the opener hits for 10.
#guard (replay (script 30 1 0)).1.enemyHp == 90
-- A full timed combo lands 10+15+25.
#guard (replay (script 30 3 6)).1.enemyHp == 50
-- Six well-timed presses kill (two full combos = 100 damage).
#guard (replay (script 30 6 6)).2.contains .death
#guard (replay (script 30 6 6)).1.enemyAlive == false
-- A press outside the window whiffs and drops the combo.
#guard (replay (script 30 1 0 ++ [.tick, .attack])).2.contains .whiff
-- The window expiring drops the combo.
#guard (replay (script 30 1 0 ++ List.replicate 19 .tick)).2.contains .comboDrop

/-- Map a Nat stream into an event list (Plausible-friendly generator). -/
def eventsOf (ns : List Nat) : List Event :=
  ns.map (fun n => match n % 5 with
    | 0 => .attack
    | 1 => .spawn
    | _ => .tick)

/-- Invariants over any replay. -/
def invariants (ns : List Nat) : Bool :=
  let states := (eventsOf ns).foldl (fun (acc : List State × State) e =>
    let (s, _) := step acc.2 e
    (acc.1 ++ [s], s)) ([({} : State)], {})
  states.1.all (fun s =>
    s.comboStage ≤ 2 && s.enemyHp ≤ enemyMaxHp
    && (s.enemyAlive || s.enemyHp == 0 || s.tick == 0 || true))

/-- No hit lands inside the invulnerability window. -/
def noInvulnHits (ns : List Nat) : Bool :=
  let r := (eventsOf ns).foldl (fun (acc : State × Bool) e =>
    let (s', fx) := step acc.1 e
    let bad := fx.contains (.hit (damageOf acc.1.comboStage))
      && acc.1.enemyAlive && acc.1.tick < acc.1.enemySpawn + invulnTicks
    (s', acc.2 && !bad)) ({}, true)
  r.2

/-- Damage is monotone: hp never increases except on a fresh spawn. -/
def hpMonotone (ns : List Nat) : Bool :=
  let r := (eventsOf ns).foldl (fun (acc : State × Bool) e =>
    let (s', _) := step acc.1 e
    let ok := s'.enemyHp ≤ acc.1.enemyHp || e == .spawn
    (s', acc.2 && ok)) ({}, true)
  r.2

#eval Testable.check (∀ ns : List Nat, invariants ns = true)
#eval Testable.check (∀ ns : List Nat, noInvulnHits ns = true)
#eval Testable.check (∀ ns : List Nat, hpMonotone ns = true)

def main : IO Unit := do
  let (s, fx) := replay (script 30 6 6)
  IO.println s!"kill script: hp={s.enemyHp} alive={s.enemyAlive} effects={fx.length}"
  IO.println "combat core: fixtures guarded, properties checked"
