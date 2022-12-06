import Advent.Advent2022

def selectChallenge (year day : Nat) : Option String := do
  let solution := match year with
  | 2022 => Advent2022.selectDay day
  | _ => none
  match solution with
  | some result => match result () with
    | EStateM.Result.ok ⟨part1, part2⟩ _ => some s!"year: {year}, day: {day} => ({part1}, {part2})"
    | EStateM.Result.error err _ => some s!"year: {year}, day: {day} => {err}"
  | none => none

def selectAllChallengesFromYear (year : Nat) : List String :=
  match selectChallenge year 1 with
  | some solution => solution :: []
  | none => []

def selectAllChallenges : List String := selectAllChallengesFromYear 2022
