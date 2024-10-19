# Read input from a text file
passport_data <- readLines("input_level_4.txt", warn = FALSE)

# Combine lines into a single string, separating passports by newlines
passport_data <- paste(passport_data, collapse = "\n")
passport_list <- strsplit(passport_data, "\n\n")[[1]]

# Validation functions for each field
validate_byr <- function(byr) { as.numeric(byr) >= 1920 & as.numeric(byr) <= 2002 }
validate_iyr <- function(iyr) { as.numeric(iyr) >= 2010 & as.numeric(iyr) <= 2020 }
validate_eyr <- function(eyr) { as.numeric(eyr) >= 2020 & as.numeric(eyr) <= 2030 }
validate_hgt <- function(hgt) {
  if (grepl("cm$", hgt)) {
    num <- as.numeric(sub("cm", "", hgt))
    return(num >= 150 & num <= 193)
  } else if (grepl("in$", hgt)) {
    num <- as.numeric(sub("in", "", hgt))
    return(num >= 59 & num <= 76)
  }
  return(FALSE)
}
validate_hcl <- function(hcl) { grepl("^#[0-9a-f]{6}$", hcl) }
validate_ecl <- function(ecl) { ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth") }
validate_pid <- function(pid) { grepl("^[0-9]{9}$", pid) }

# Function to check if a passport contains all required fields and validates them
is_valid_passport_part_two <- function(passport) {
  fields <- gsub(":", "=", passport)  # Replace : with = for easier parsing
  field_list <- strsplit(fields, "[ =\n]")[[1]]
  field_values <- setNames(field_list[seq(2, length(field_list), 2)], field_list[seq(1, length(field_list), 2)])
  
  # Check for the presence of all required fields
  required_fields <- c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
  if (!all(required_fields %in% names(field_values))) {
    return(FALSE)
  }
  
  # Validate each field
  valid_byr <- validate_byr(field_values["byr"])
  valid_iyr <- validate_iyr(field_values["iyr"])
  valid_eyr <- validate_eyr(field_values["eyr"])
  valid_hgt <- validate_hgt(field_values["hgt"])
  valid_hcl <- validate_hcl(field_values["hcl"])
  valid_ecl <- validate_ecl(field_values["ecl"])
  valid_pid <- validate_pid(field_values["pid"])
  
  # Return TRUE if all fields are valid
  return(valid_byr & valid_iyr & valid_eyr & valid_hgt & valid_hcl & valid_ecl & valid_pid)
}

# Apply the validation function to each passport
valid_passports <- sapply(passport_list, is_valid_passport_part_two)

# Count how many passports are valid
result <- sum(valid_passports)

# Output the result
print(result)
