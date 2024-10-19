# Read input from a text file
passport_data <- readLines("input_level_4.txt", warn = FALSE)

# Combine lines into a single string, separating passports by newlines
passport_data <- paste(passport_data, collapse = "\n")
passport_list <- strsplit(passport_data, "\n\n")[[1]]

# Function to check if a passport contains all required fields
is_valid_passport <- function(passport) {
  required_fields <- c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
  
  # Create a list of fields present in the passport
  fields <- gsub(":", "=", passport)  # Replace : with = for easier parsing
  field_list <- strsplit(fields, "[ =\n]")[[1]]
  
  # Check if all required fields are present
  all(required_fields %in% field_list)
}

# Apply the validation function to each passport
valid_passports <- sapply(passport_list, is_valid_passport)

# Count how many passports are valid
result <- sum(valid_passports)

# Output the result
print(result)
