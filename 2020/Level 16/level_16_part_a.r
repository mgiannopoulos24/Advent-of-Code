# Read input file
input <- readLines("input_level_16.txt", warn = FALSE)

# Separate sections in the input
rules_section <- input[1:grep("your ticket:", input) - 1]
your_ticket_section <- input[(grep("your ticket:", input) + 1):(grep("nearby tickets:", input) - 1)]
nearby_tickets_section <- input[(grep("nearby tickets:", input) + 1):length(input)]

# Parse field rules
rules <- list()
for (line in rules_section) {
  rule_name <- sub(":.*", "", line)
  ranges <- regmatches(line, gregexpr("\\d+-\\d+", line))[[1]]
  if (length(ranges) == 2) {  # Ensure there are exactly two ranges
    range1 <- as.numeric(unlist(strsplit(ranges[1], "-")))
    range2 <- as.numeric(unlist(strsplit(ranges[2], "-")))
    if (!any(is.na(range1)) && !any(is.na(range2))) {  # Ensure ranges are numeric
      rules[[rule_name]] <- list(range1, range2)
    }
  }
}

# Function to check if a value is valid for any field
is_valid_for_any_field <- function(value, rules) {
  for (rule in rules) {
    if ((value >= rule[[1]][1] && value <= rule[[1]][2]) ||
        (value >= rule[[2]][1] && value <= rule[[2]][2])) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# Parse nearby tickets and calculate error rate
error_rate <- 0
for (ticket_line in nearby_tickets_section) {
  values <- as.numeric(unlist(strsplit(ticket_line, ",")))
  for (value in values) {
    if (!is.na(value) && !is_valid_for_any_field(value, rules)) {
      error_rate <- error_rate + value
    }
  }
}

# Output the ticket scanning error rate
print(error_rate)
