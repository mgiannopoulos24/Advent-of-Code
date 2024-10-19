# Read input from a text file
boarding_passes <- readLines("input_level_5.txt", warn = FALSE)

# Function to convert a boarding pass to row and column
decode_seat <- function(boarding_pass) {
  # Convert F to 0, B to 1 for the row
  row_binary <- gsub("F", "0", gsub("B", "1", substr(boarding_pass, 1, 7)))
  row <- strtoi(row_binary, base = 2)
  
  # Convert L to 0, R to 1 for the column
  col_binary <- gsub("L", "0", gsub("R", "1", substr(boarding_pass, 8, 10)))
  col <- strtoi(col_binary, base = 2)
  
  # Calculate the seat ID
  seat_id <- row * 8 + col
  return(seat_id)
}

# Decode all boarding passes and find the highest seat ID
seat_ids <- sapply(boarding_passes, decode_seat)
max_seat_id <- max(seat_ids)

# Output the result
print(max_seat_id)
