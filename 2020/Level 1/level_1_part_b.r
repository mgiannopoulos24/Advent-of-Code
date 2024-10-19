# Read input from a text file
expense_report <- as.numeric(readLines("input_level_1.txt", warn = FALSE))

# Function to find three entries that sum to 2020 and multiply them together
find_three_entries <- function(report, target_sum) {
  for (i in 1:(length(report) - 2)) {
    for (j in (i + 1):(length(report) - 1)) {
      for (k in (j + 1):length(report)) {
        if (report[i] + report[j] + report[k] == target_sum) {
          return(report[i] * report[j] * report[k])
        }
      }
    }
  }
  return(NA) # Return NA if no triplet is found
}

# Find the three entries that sum to 2020
result <- find_three_entries(expense_report, 2020)

# Output the result
print(result)
