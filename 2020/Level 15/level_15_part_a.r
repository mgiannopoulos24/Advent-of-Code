# Puzzle input
starting_numbers <- c(0, 6, 1, 7, 2, 19, 20)

# Initialize memory to store the last spoken turn of each number
memory <- list()
last_spoken <- 0

# Set up the initial turns with the starting numbers
for (turn in 1:(length(starting_numbers) - 1)) {
  memory[[as.character(starting_numbers[turn])]] <- turn
}
last_spoken <- tail(starting_numbers, n = 1)

# Play the game until the 2020th turn
for (turn in (length(starting_numbers) + 1):2020) {
  if (!is.null(memory[[as.character(last_spoken)]])) {
    new_spoken <- turn - 1 - memory[[as.character(last_spoken)]]
  } else {
    new_spoken <- 0
  }
  memory[[as.character(last_spoken)]] <- turn - 1
  last_spoken <- new_spoken
}

# Output the 2020th number spoken
print(last_spoken)
