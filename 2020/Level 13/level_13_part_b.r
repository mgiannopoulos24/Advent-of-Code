# Read the input from the text file
input <- readLines("input_level_13.txt", warn = FALSE)

# Parse the bus IDs and their respective offsets
bus_ids_with_x <- unlist(strsplit(input[2], ","))
bus_ids <- as.numeric(bus_ids_with_x[bus_ids_with_x != "x"])
offsets <- which(bus_ids_with_x != "x") - 1

# Function to find the earliest timestamp using the Chinese Remainder Theorem approach
find_earliest_timestamp <- function(bus_ids, offsets) {
  timestamp <- 0
  step <- bus_ids[1]
  
  for (i in 2:length(bus_ids)) {
    bus_id <- bus_ids[i]
    offset <- offsets[i]
    
    # Increment the timestamp until we find a value that satisfies the condition
    while ((timestamp + offset) %% bus_id != 0) {
      timestamp <- timestamp + step
      
      # To avoid infinite loops or potential issues, check for overflow
      if (is.na(timestamp)) {
        stop("Integer overflow detected, switching to numeric (double) precision.")
      }
    }
    
    # Increase the step by the bus ID to ensure the solution holds for previous buses
    step <- step * bus_id
    
    # Check for potential overflow
    if (is.na(step)) {
      stop("Step size overflow detected. Please verify bus IDs.")
    }
  }
  
  return(timestamp)
}

# Find the earliest timestamp
earliest_timestamp <- find_earliest_timestamp(bus_ids, offsets)

# Set options to prevent scientific notation
options(scipen = 999)

# Output the result
print(earliest_timestamp)
