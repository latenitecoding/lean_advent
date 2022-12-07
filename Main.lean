import Advent

def printSolutions (solutions : List (Nat × Nat × String × String)) : IO Unit := do
  match solutions with
  | [] => pure ()
  | solution :: solutions =>
    match solution with
    | (year, day, part1, part2) =>
      IO.println s!"fun \{year = {year}}, \{day = {day}} => ({part1}, {part2})"
    printSolutions solutions

def main (args : List String) : IO UInt32 := do
  let solutions ← match args with
    | "--year" :: (year :: ("--day" :: (day :: [])))
    | "--day" :: (day :: ("--year" :: (year :: []))) =>
      match ←(Advent.selectChallenge year.toNat! day.toNat!) with
      | some (part1, part2) => pure $ (year.toNat!, day.toNat!, part1, part2) :: []
      | none => pure []
    | "--year" :: (year :: []) =>
      Advent.selectAllChallengesFromYear (year.toNat!)
        |>.map (fun l => l.map (fun t => (year.toNat!, t)))
    | "--day" :: (day :: []) =>
      Advent.selectAllChallengesWithDay (day.toNat!)
        |>.map (fun l =>
          l.map (fun t =>
            match t with
            | (year, part1, part2) => (year, day.toNat!, part1, part2)
          ))
    | _ => Advent.selectAllChallenges
  match solutions with
  | [] => IO.println "Sorry! No solutions..."
  | _ => printSolutions solutions
  pure 0
