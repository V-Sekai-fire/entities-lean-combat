import CombatCore
open CombatCore

def evName : Event → String
  | .tick => "tick" | .attack => "attack" | .spawn => "spawn"

def fxName : Effect → String
  | .swing s => s!"swing{s}"
  | .hit d => s!"hit{d}"
  | .blocked => "blocked"
  | .whiff => "whiff"
  | .comboDrop => "comboDrop"
  | .death => "death"

def script : List Event :=
  [.spawn] ++ List.replicate 30 .tick
    ++ (List.range 6).flatMap (fun i =>
        if i == 0 then [.attack] else List.replicate 6 .tick ++ [.attack])
    ++ [.attack]  -- a press after death still swings at air
    ++ List.replicate 20 .tick

def main : IO Unit := do
  IO.FS.createDirAll "build"
  let lines := (script.foldl (fun (acc : State × List String) e =>
    let (s', fx) := step acc.1 e
    let fxs := if fx.isEmpty then "-" else String.intercalate "+" (fx.map fxName)
    (s', acc.2 ++ [s!"{evName e};{fxs}"])) ({}, [])).2
  IO.FS.writeFile "build/combat_golden.csv" (String.intercalate "\n" lines ++ "\n")
  IO.println s!"wrote build/combat_golden.csv ({lines.length} events)"
