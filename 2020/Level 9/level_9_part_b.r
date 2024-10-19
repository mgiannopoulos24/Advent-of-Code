# Read input from a text file
xmas_data <- as.numeric(readLines("input_level_9.txt", warn = FALSE))

# Function to check if a number is the sum of any two numbers in a list (Part One)
is_valid <- function(preamble, target) {
  for (i in 1:(length(preamble) - 1)) {
    for (j in (i + 1):length(preamble)) {
      if (preamble[i] + preamble[j] == target) {
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

# Function to find the first invalid number after the preamble (Part One)
find_invalid_number <- function(data, preamble_length) {
  for (i in (preamble_length + 1):length(data)) {
    preamble <- data[(i - preamble_length):(i - 1)]
    target <- data[i]
    if (!is_valid(preamble, target)) {
      return(target)
    }
  }
  return(NA) # Return NA if all numbers are valid
}

# Part Two: Find the contiguous set of numbers that sum to the invalid number
find_contiguous_set <- function(data, target) {
  for (i in 1:(length(data) - 1)) {
    sum <- 0
    for (j in i:length(data)) {
      sum <- sum + data[j]
      if (sum == target) {
        return(data[i:j])
      } else if (sum > target) {
        break
      }
    }
  }
  return(NULL)
}

# Define the preamble length (25 for this puzzle)
preamble_length <- 25

# Find the first invalid number in the data (Part One)
invalid_number <- find_invalid_number(xmas_data, preamble_length)

# Find the contiguous set of numbers that sum to the invalid number (Part Two)
contiguous_set <- find_contiguous_set(xmas_data, invalid_number)

# Calculate the encryption weakness (smallest + largest number in the contiguous set)
encryption_weakness <- min(contiguous_set) + max(contiguous_set)

# Output the result
print(encryption_weakness)
