# Read input from a text file
seat_layout <- readLines("input_level_11.txt", warn = FALSE)

# Convert the seat layout to a matrix
seat_matrix <- do.call(rbind, strsplit(seat_layout, ""))

# Function to get the adjacent seats around a specific seat (handles edges)
get_adjacent_seats <- function(seat_matrix, row, col) {
  n_rows <- nrow(seat_matrix)
  n_cols <- ncol(seat_matrix)
  
  adjacent <- c()
  for (i in (row - 1):(row + 1)) {
    for (j in (col - 1):(col + 1)) {
      if (i > 0 && i <= n_rows && j > 0 && j <= n_cols && !(i == row && j == col)) {
        adjacent <- c(adjacent, seat_matrix[i, j])
      }
    }
  }
  return(adjacent)
}

# Function to apply the seating rules for one round
apply_seating_rules <- function(seat_matrix) {
  new_seat_matrix <- seat_matrix
  
  for (row in 1:nrow(seat_matrix)) {
    for (col in 1:ncol(seat_matrix)) {
      current_seat <- seat_matrix[row, col]
      adjacent_seats <- get_adjacent_seats(seat_matrix, row, col)
      
      if (current_seat == "L" && all(adjacent_seats != "#")) {
        new_seat_matrix[row, col] <- "#"
      } else if (current_seat == "#" && sum(adjacent_seats == "#") >= 4) {
        new_seat_matrix[row, col] <- "L"
      }
    }
  }
  
  return(new_seat_matrix)
}

# Function to simulate until the seating arrangement stabilizes
simulate_seating <- function(seat_matrix) {
  repeat {
    new_seat_matrix <- apply_seating_rules(seat_matrix)
    if (all(new_seat_matrix == seat_matrix)) {
      break
    }
    seat_matrix <- new_seat_matrix
  }
  return(seat_matrix)
}

# Run the simulation
final_seat_matrix <- simulate_seating(seat_matrix)

# Count the number of occupied seats
occupied_seats <- sum(final_seat_matrix == "#")

# Output the result
print(occupied_seats)
