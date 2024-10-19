# Read input from a text file
map_data <- readLines("input_level_3.txt", warn = FALSE)

# Function to count trees encountered for a given slope
count_trees <- function(map, right_step, down_step) {
  num_rows <- length(map)
  num_cols <- nchar(map[1])
  
  row <- 1
  col <- 1
  tree_count <- 0
  
  while (row <= num_rows) {
    # Check if current position is a tree (#)
    if (substr(map[row], col, col) == "#") {
      tree_count <- tree_count + 1
    }
    
    # Move right and down according to the slope
    col <- (col + right_step - 1) %% num_cols + 1
    row <- row + down_step
  }
  
  return(tree_count)
}

# Define the slopes to check
slopes <- list(
  c(1, 1),  # Right 1, down 1
  c(3, 1),  # Right 3, down 1
  c(5, 1),  # Right 5, down 1
  c(7, 1),  # Right 7, down 1
  c(1, 2)   # Right 1, down 2
)

# Calculate the number of trees for each slope and multiply the results together
tree_counts <- sapply(slopes, function(slope) count_trees(map_data, slope[1], slope[2]))
result <- prod(tree_counts)

# Output the result
print(result)
