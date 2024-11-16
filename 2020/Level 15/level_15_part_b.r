# Puzzle input
starting_numbers <- c(0, 6, 1, 7, 2, 19, 20)

# Initialize memory for storing last spoken turn of each number
memory <- integer(30000000)
last_spoken <- 0

# Set up initial turns with starting numbers
for (turn in 1:(length(starting_numbers) - 1)) {
  memory[starting_numbers[turn] + 1] <- turn
}
last_spoken <- tail(starting_numbers, n = 1)

# Play the game until the 30,000,000th turn
for (turn in (length(starting_numbers) + 1):30000000) {
  if (memory[last_spoken + 1] != 0) {
    new_spoken <- turn - 1 - memory[last_spoken + 1]
  } else {
    new_spoken <- 0
  }
  memory[last_spoken + 1] <- turn - 1
  last_spoken <- new_spoken
}

# Output the 30,000,000th number spoken
print(last_spoken)
