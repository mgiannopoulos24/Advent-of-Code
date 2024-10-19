# Read input from a text file
password_data <- readLines("input_level_2.txt", warn = FALSE)

# Function to parse each line and check if the password is valid
is_valid_password_part_two <- function(line) {
  # Extract the parts of the line using regex
  parts <- strcapture("^([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)$", line, 
                      proto = list(pos1 = integer(), pos2 = integer(), letter = character(), password = character()))
  
  # Extract characters at the specified positions (adjust for 1-based index)
  char1 <- substr(parts$password, parts$pos1, parts$pos1)
  char2 <- substr(parts$password, parts$pos2, parts$pos2)
  
  # Check if exactly one of the positions contains the required letter
  return((char1 == parts$letter) != (char2 == parts$letter))  # XOR operation
}

# Apply the validation function to each line of the password data
valid_passwords <- sapply(password_data, is_valid_password_part_two)

# Count how many passwords are valid
result <- sum(valid_passwords)

# Output the result
print(result)
