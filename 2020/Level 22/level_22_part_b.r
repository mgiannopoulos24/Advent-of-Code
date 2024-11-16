# Read input from a text file
data <- readLines("input_level_22.txt", warn = FALSE)

# Parse the decks
deck1 <- as.integer(data[2:(which(data == "") - 1)])
deck2 <- as.integer(data[(which(data == "") + 2):length(data)])

# Recursive Combat Function
play_recursive_combat <- function(deck1, deck2) {
  previous_rounds <- list()

  while (length(deck1) > 0 && length(deck2) > 0) {
    # Check for repeated configuration
    current_state <- paste(c(deck1, "|", deck2), collapse = ",")
    if (current_state %in% previous_rounds) {
      return(list(winner = 1, deck = deck1))
    }
    previous_rounds <- c(previous_rounds, current_state)

    # Draw cards
    card1 <- deck1[1]
    card2 <- deck2[1]
    deck1 <- deck1[-1]
    deck2 <- deck2[-1]

    # Determine round winner
    if (length(deck1) >= card1 && length(deck2) >= card2) {
      # Play sub-game
      subgame_result <- play_recursive_combat(deck1[1:card1], deck2[1:card2])
      winner <- subgame_result$winner
    } else {
      winner <- if (card1 > card2) 1 else 2
    }

    # Update decks based on winner
    if (winner == 1) {
      deck1 <- c(deck1, card1, card2)
    } else {
      deck2 <- c(deck2, card2, card1)
    }
  }

  if (length(deck1) > 0) {
    return(list(winner = 1, deck = deck1))
  } else {
    return(list(winner = 2, deck = deck2))
  }
}

# Calculate the score of a deck
calculate_score <- function(deck) {
  score <- sum(deck * rev(seq_along(deck)))
  return(score)
}

# Play Recursive Combat and calculate the score
result <- play_recursive_combat(deck1, deck2)
winning_score <- calculate_score(result$deck)

# Output the result
cat(winning_score, "\n")
