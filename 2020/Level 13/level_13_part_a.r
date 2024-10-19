# Read the input from a text file
input <- readLines("input_level_13.txt", warn = FALSE)

# Parse the input
earliest_timestamp <- as.integer(input[1])
bus_ids <- unlist(strsplit(input[2], ","))

# Filter out 'x' entries and convert bus IDs to numeric
bus_ids <- as.integer(bus_ids[bus_ids != "x"])

# Function to calculate the wait time for each bus
get_wait_time <- function(bus_id, timestamp) {
  # Next departure time for the bus
  next_departure <- ceiling(timestamp / bus_id) * bus_id
  return(next_departure - timestamp)
}

# Calculate wait times for all bus IDs
wait_times <- sapply(bus_ids, get_wait_time, timestamp = earliest_timestamp)

# Find the bus with the minimum wait time
min_wait_index <- which.min(wait_times)
earliest_bus_id <- bus_ids[min_wait_index]
min_wait_time <- wait_times[min_wait_index]

# Calculate the result: bus ID * wait time
result <- earliest_bus_id * min_wait_time

# Output the result
print(result)
