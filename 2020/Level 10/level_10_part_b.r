# Read input from a text file
adapter_ratings <- as.numeric(readLines("input_level_10.txt", warn = FALSE))

# Function to calculate the total number of distinct ways to arrange adapters
calculate_arrangements <- function(adapter_ratings) {
  # Add 0 for the charging outlet and the device's built-in adapter (max + 3)
  adapter_ratings <- sort(c(0, adapter_ratings, max(adapter_ratings) + 3))
  
  # Create a vector to store the number of ways to reach each adapter
  ways <- numeric(length(adapter_ratings))
  ways[1] <- 1  # There's only 1 way to start at the charging outlet (0 jolts)
  
  # Loop through the adapters and calculate the number of ways to reach each one
  for (i in 2:length(adapter_ratings)) {
    for (j in (i - 1):1) {
      if (adapter_ratings[i] - adapter_ratings[j] > 3) {
        break  # No need to check further if the difference exceeds 3
      }
      ways[i] <- ways[i] + ways[j]
    }
  }
  
  # The total number of ways to connect to the last adapter is the result
  return(ways[length(ways)])
}

# Calculate the total number of distinct arrangements
result <- calculate_arrangements(adapter_ratings)

# Output the result
print(format(result, scientific = FALSE))
