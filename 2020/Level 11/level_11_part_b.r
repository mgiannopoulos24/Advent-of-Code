# Read input from a text file
seat_layout <- readLines("input_level_11.txt", warn = FALSE)

# Convert the seat layout to a matrix
seat_matrix <- do.call(rbind, strsplit(seat_layout, ""))

# Function to get the first visible seat in each direction
get_visible_seats <- function(seat_matrix, row, col) {
  n_rows <- nrow(seat_matrix)
  n_cols <- ncol(seat_matrix)
  
  directions <- list(c(-1, -1), c(-1, 0), c(-1, 1), c(0, -1), c(0, 1), c(1, -1), c(1, 0), c(1, 1))
  visible_seats <- c()
  
  for (dir in directions) {
    r <- row + dir[1]
    c <- col + dir[2]
    
    while (r > 0 && r <= n_rows && c > 0 && c <= n_cols) {
      if (seat_matrix[r, c] != ".") {
        visible_seats <- c(visible_seats, seat_matrix[r, c])
        break
      }
      r <- r + dir[1]
      c <- c + dir[2]
    }
  }
  
  return(visible_seats)
}

# Function to apply the seating rules for one round
apply_seating_rules <- function(seat_matrix) {
  new_seat_matrix <- seat_matrix
  
  for (row in 1:nrow(seat_matrix)) {
    for (col in 1:ncol(seat_matrix)) {
      current_seat <- seat_matrix[row, col]
      if (current_seat == ".") next
      
      visible_seats <- get_visible_seats(seat_matrix, row, col)
      
      if (current_seat == "L" && all(visible_seats != "#")) {
        new_seat_matrix[row, col] <- "#"
      } else if (current_seat == "#" && sum(visible_seats == "#") >= 5) {
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
