# Read input from a text file
password_data <- readLines("input_level_2.txt", warn = FALSE)

# Function to parse each line and check if the password is valid
is_valid_password <- function(line) {
  # Extract the parts of the line using regex
  parts <- strcapture("^([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)$", line, 
                      proto = list(min_count = integer(), max_count = integer(), letter = character(), password = character()))
  
  # Count the occurrences of the required letter in the password
  letter_count <- sum(strsplit(parts$password, "")[[1]] == parts$letter)
  
  # Check if the letter count is within the allowed range
  return(letter_count >= parts$min_count & letter_count <= parts$max_count)
}

# Apply the validation function to each line of the password data
valid_passwords <- sapply(password_data, is_valid_password)

# Count how many passwords are valid
result <- sum(valid_passwords)

# Output the result
print(result)
