import Advent.Advent2022

def selectChallenge (year day : Nat) : IO (Option String) := do
  let solution ← match year with
  | 2022 => Advent2022.selectDay day
  | _ => pure none
  match solution with
  | some ⟨part1, part2⟩ => pure (some s!"year: {year}, day: {day} => ({part1}, {part2})")
  | none => pure none

def selectAllChallengesFromYear (year : Nat) : IO (List String) := do
  match ←(selectChallenge year 1) with
  | some solution => pure (solution :: [])
  | none => pure []

def selectAllChallenges : IO (List String) := do
  selectAllChallengesFromYear 2022
