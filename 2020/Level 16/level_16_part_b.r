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

# Parse your ticket
your_ticket <- as.numeric(unlist(strsplit(your_ticket_section, ",")))

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

# Filter valid tickets
valid_tickets <- list()
for (ticket_line in nearby_tickets_section) {
  values <- as.numeric(unlist(strsplit(ticket_line, ",")))
  if (all(sapply(values, is_valid_for_any_field, rules = rules))) {
    valid_tickets <- append(valid_tickets, list(values))
  }
}

# Determine possible field positions for each rule
num_fields <- length(your_ticket)
possible_positions <- lapply(rules, function(rule) seq_len(num_fields))

for (ticket in valid_tickets) {
  for (pos in seq_len(num_fields)) {
    value <- ticket[pos]
    for (rule_name in names(rules)) {
      rule <- rules[[rule_name]]
      if (!((value >= rule[[1]][1] && value <= rule[[1]][2]) ||
            (value >= rule[[2]][1] && value <= rule[[2]][2]))) {
        possible_positions[[rule_name]] <- setdiff(possible_positions[[rule_name]], pos)
      }
    }
  }
}

# Narrow down positions to determine the exact order of fields
field_positions <- rep(NA, num_fields)
while (any(sapply(possible_positions, length) > 0)) {
  for (rule_name in names(possible_positions)) {
    if (length(possible_positions[[rule_name]]) == 1) {
      pos <- possible_positions[[rule_name]]
      field_positions[pos] <- rule_name
      possible_positions <- lapply(possible_positions, setdiff, pos)
    }
  }
}

# Find the positions of "departure" fields in your ticket and multiply their values
departure_values <- your_ticket[grep("^departure", field_positions)]
result <- prod(departure_values)
print(result)
