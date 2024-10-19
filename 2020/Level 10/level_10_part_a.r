# Read input from a text file
adapter_ratings <- as.numeric(readLines("input_level_10.txt", warn = FALSE))

# Function to calculate the number of 1-jolt and 3-jolt differences
calculate_joltage_differences <- function(adapter_ratings) {
  # Add 0 for the charging outlet and the device's built-in adapter (max + 3)
  adapter_ratings <- sort(c(0, adapter_ratings, max(adapter_ratings) + 3))
  
  # Calculate the differences between consecutive adapters
  differences <- diff(adapter_ratings)
  
  # Count the number of 1-jolt and 3-jolt differences
  num_1_jolt <- sum(differences == 1)
  num_3_jolt <- sum(differences == 3)
  
  return(num_1_jolt * num_3_jolt)
}

# Calculate the product of the number of 1-jolt and 3-jolt differences
result <- calculate_joltage_differences(adapter_ratings)

# Output the result
print(result)
