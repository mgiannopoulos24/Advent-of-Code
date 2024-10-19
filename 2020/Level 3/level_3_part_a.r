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

# Define the slope: right 3, down 1
right_step <- 3
down_step <- 1

# Count the number of trees encountered
result <- count_trees(map_data, right_step, down_step)

# Output the result
print(result)
