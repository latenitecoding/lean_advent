import Advent

def printSolutions (solutions : List String) : IO UInt32 := do
  match solutions with
  | [] =>
    IO.println "Sorry! No solutions..."
    pure 1
  | solution :: [] =>
    IO.println solution
    pure 0
  | solution :: solutions =>
    IO.println solution
    printSolutions solutions

def main (args : List String) : IO UInt32 := do
  printSolutions (
    match args with
    | _ => selectAllChallenges
  )
