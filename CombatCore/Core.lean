namespace CombatCore

/-- Tuning constants in server ticks (constant-step clock, `tick_source`). -/
def comboMinGap : UInt32 := 6    -- earliest a chained press lands
def comboMaxGap : UInt32 := 18   -- latest a chained press lands
def invulnTicks : UInt32 := 30   -- enemy spawn-invulnerability window
def enemyMaxHp  : UInt32 := 100

/-- Damage per combo stage (the three-button rhythm). -/
def damageOf : UInt32 → UInt32
  | 0 => 10
  | 1 => 15
  | _ => 25

/-- The combat state: a pure value the reducer threads. All integers, so replay
    is bit-exact (the determinism decision). -/
structure State where
  tick       : UInt32 := 0
  comboStage : UInt32 := 0   -- 0..2 = the next strike index
  lastAttack : UInt32 := 0   -- tick of the last landed press
  enemyHp    : UInt32 := 0
  enemySpawn : UInt32 := 0
  enemyAlive : Bool   := false
  deriving DecidableEq, Repr, Inhabited

inductive Event
  | tick    -- the constant-step clock advances
  | attack  -- a player press (input_source)
  | spawn   -- the enemy spawns (behavior_source)
  deriving DecidableEq, Repr

inductive Effect
  | swing (stage : UInt32)   -- the press lands a swing at this combo stage
  | hit (damage : UInt32)    -- the swing connects
  | blocked                  -- the swing meets the invulnerability window
  | whiff                    -- the press misses its timing window
  | comboDrop                -- the window expires with no press
  | death                    -- the enemy falls (event_sink: doors unlock)
  deriving DecidableEq, Repr

/-- Resolve a landed swing against the enemy. -/
def resolveSwing (s : State) (stage : UInt32) : State × List Effect :=
  if !s.enemyAlive then (s, [.swing stage])
  else if s.tick < s.enemySpawn + invulnTicks then (s, [.swing stage, .blocked])
  else
    let dmg := damageOf stage
    if s.enemyHp ≤ dmg then
      ({ s with enemyHp := 0, enemyAlive := false }, [.swing stage, .hit dmg, .death])
    else
      ({ s with enemyHp := s.enemyHp - dmg }, [.swing stage, .hit dmg])

/-- The pure reducer: `step : State → Event → State × Effects`. -/
def step (s : State) : Event → State × List Effect
  | .tick =>
    let s := { s with tick := s.tick + 1 }
    if s.comboStage > 0 && s.tick > s.lastAttack + comboMaxGap then
      ({ s with comboStage := 0 }, [.comboDrop])
    else (s, [])
  | .spawn =>
    ({ s with enemyAlive := true, enemyHp := enemyMaxHp, enemySpawn := s.tick }, [])
  | .attack =>
    if s.comboStage == 0 then
      -- An opener always swings.
      let (s', fx) := resolveSwing { s with comboStage := 1, lastAttack := s.tick } 0
      (s', fx)
    else
      let gap := s.tick - s.lastAttack
      if comboMinGap ≤ gap && gap ≤ comboMaxGap then
        let stage := s.comboStage
        let next := if stage ≥ 2 then 0 else stage + 1
        let (s', fx) := resolveSwing { s with comboStage := next, lastAttack := s.tick } stage
        (s', fx)
      else
        ({ s with comboStage := 0 }, [.whiff])

/-- Replay an event log from the empty state. -/
def replay (events : List Event) : State × List Effect :=
  events.foldl (fun (acc : State × List Effect) e =>
    let (s, fx) := step acc.1 e
    (s, acc.2 ++ fx)) ({}, [])

end CombatCore
