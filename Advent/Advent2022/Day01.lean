
def Day01.parseInput (input : String) : Array Nat := Id.run do
  input
    |>.splitOn "\n\n"
    |>.map (· |>.splitOn "\n"
              |>.map (·.toNat!)
              |>.foldl (· + ·) 0
      )
    |>.toArray

/-
The jungle must be too overgrown and difficult to navigate in vehicles or
access from the air; the Elves' expedition traditionally goes on foot. As
your boats approach land, the Elves begin taking inventory of their supplies.
One important consideration is food - in particular, the number of Calories
each Elf is carrying (your puzzle input).

The Elves take turns writing down the number of Calories contained by the
various meals, snacks, rations, etc. that they've brought with them, one
item per line. Each Elf separates their own inventory from the previous
Elf's inventory (if any) by a blank line.

PART 1 : Find the Elf carrying the most Calories. How many total Calories
is that Elf carrying?
-/
def Day01.part1 (input : String) : String := Id.run do
  let maxElf ← Day01.parseInput input
    |>.foldl (fun a b : Nat => if a ≥ b then a else b) 0
  s!"{maxElf}"

/-
PART 2 : Find the top three Elves carrying the most Calories. How many
Calories are those Elves carrying in total?
-/
def Day01.part2 (input : String) : String := Id.run do
  let maxElvesSum ← Day01.parseInput input
    |>.qsort (· ≥ ·)
    |>.extract 0 3
    |>.foldl (fun a b : Nat => a + b) 0
  s!"{maxElvesSum}"

def Day01.solve : IO (String × String) := do
  let input ← IO.FS.readFile ⟨"inputs/y2022d01.txt"⟩
  pure ⟨Day01.part1 input, Day01.part2 input⟩
