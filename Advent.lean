import Advent.Advent2022

def Advent.selectChallenge (year day : Nat) : IO (Option (String × String)) :=
  match year with
  | 2022 => Advent2022.selectDay day
  | _ => pure none

def Advent.selectAllChallengesFromYear (year : Nat) : IO (List (Nat × String × String)) :=
  Std.Range.mk 1 26 1
    |>.forIn [] (fun day solutions => do
        match day with
        | _ => match ←(selectChallenge year day) with 
          | some (part1, part2) => pure $ ForInStep.yield $ (day, part1, part2) :: solutions
          | none => pure $ ForInStep.yield solutions
      )

def Advent.selectAllChallengesWithDay (day : Nat) : IO (List (Nat × String × String)) :=
  Std.Range.mk 2015 2023 1
    |>.forIn [] (fun year solutions => do
        match day with
        | _ => match ←(selectChallenge year day) with 
          | some (part1, part2) => pure $ ForInStep.yield $ (year, part1, part2) :: solutions
          | none => pure $ ForInStep.yield solutions
      )

def Advent.selectAllChallenges : IO (List (Nat × Nat × String × String)) := do
  Std.Range.mk 2015 2023 1
    |>.forIn [] (fun year solutions => do
        match year with
        | _ => match ←(selectAllChallengesFromYear year) with
          | solves => pure $ ForInStep.yield $ (solves.map (fun t => (year, t))) ++ solutions
      )
