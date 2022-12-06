import Advent.Advent2022.Day01

def Advent2022.selectDay (day : Nat) : Option (IO (String × String)) :=
  match day with
  | 1 => some Day01.solve
  | _ => none
