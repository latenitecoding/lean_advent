import Advent.Advent2022.Day01

def Advent2022.selectDay (day : Nat) : IO (Option (String × String)) := do
  match day with
  | 1 => pure $ Day01.solve $ ←(IO.FS.readFile ⟨"inputs/y2022d01.txt"⟩)
  | _ => pure none
