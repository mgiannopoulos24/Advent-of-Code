# Read input from a text file
expense_report <- as.numeric(readLines("input_level_1.txt", warn = FALSE))

# Function to find two entries that sum to 2020 and multiply them together
find_entries <- function(report, target_sum) {
  for (i in 1:length(report)) {
    for (j in (i + 1):length(report)) {
      if (report[i] + report[j] == target_sum) {
        return(report[i] * report[j])
      }
    }
  }
  return(NA) # Return NA if no pair is found
}

# Find the two entries that sum to 2020
result <- find_entries(expense_report, 2020)

# Output the result
print(result)
