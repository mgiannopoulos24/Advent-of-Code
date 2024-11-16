# Read input from a text file
data <- readLines("input_level_22.txt", warn = FALSE)

# Parse the decks
deck1 <- as.integer(data[2:(which(data == "") - 1)])
deck2 <- as.integer(data[(which(data == "") + 2):length(data)])

# Function to play the game
play_combat <- function(deck1, deck2) {
  while (length(deck1) > 0 && length(deck2) > 0) {
    card1 <- deck1[1]
    card2 <- deck2[1]

    deck1 <- deck1[-1]
    deck2 <- deck2[-1]

    if (card1 > card2) {
      deck1 <- c(deck1, card1, card2)
    } else {
      deck2 <- c(deck2, card2, card1)
    }
  }

  if (length(deck1) > 0) {
    return(deck1)
  } else {
    return(deck2)
  }
}

# Calculate the score of a deck
calculate_score <- function(deck) {
  score <- sum(deck * rev(seq_along(deck)))
  return(score)
}

# Play the game and calculate the score
winning_deck <- play_combat(deck1, deck2)
winning_score <- calculate_score(winning_deck)

# Output the result
print(winning_score)
