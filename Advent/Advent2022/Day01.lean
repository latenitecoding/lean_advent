def Day01.parseInput (input : String) : Array Nat := Id.run do
  input
    |>.trim
    |>.splitOn "\n\n"
    |>.map (· |>.splitOn "\n"
              |>.map (· |>.trim
                        |>.toNat!
                )
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
def Day01.part1 (input : String) : String :=
  Day01.parseInput input
    |>.foldl (fun a b : Nat => if a ≥ b then a else b) 0
    |>.repr

/-
PART 2 : Find the top three Elves carrying the most Calories. How many
Calories are those Elves carrying in total?
-/
def Day01.part2 (input : String) : String :=
  Day01.parseInput input
    |>.qsort (· ≥ ·)
    |>.extract 0 3
    |>.foldl (fun a b : Nat => a + b) 0
    |>.repr

def Day01.solve (input : String) : String × String := ⟨Day01.part1 input, Day01.part2 input⟩

#eval Day01.part1 "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"

#eval Day01.part2 "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"
