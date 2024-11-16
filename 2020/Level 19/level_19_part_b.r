parse_input <- function(filename) {
  lines <- readLines(filename, warn = FALSE)
  blank_line_index <- which(lines == "")
  
  # Separate rules and messages
  rules_lines <- lines[1:(blank_line_index - 1)]
  messages <- lines[(blank_line_index + 1):length(lines)]
  
  # Parse rules into a list
  rules <- list()
  for (line in rules_lines) {
    parts <- strsplit(line, ": ")[[1]]
    rule_num <- as.integer(parts[1])
    rules[[as.character(rule_num)]] <- parts[2]
  }
  
  return(list(rules = rules, messages = messages))
}

build_regex <- function(rules, rule_num, depth = 5) {
  rule <- rules[[as.character(rule_num)]]
  
  # If rule is a literal (e.g., "a" or "b"), return it directly
  if (grepl("^\"[ab]\"$", rule)) {
    return(substr(rule, 2, 2))  # Remove quotes around character
  }
  
  # Specific handling for rules 8 and 11 with recursive patterns
  if (rule_num == 8) {
    # Rule 8 is effectively rule 42 repeated 1 or more times
    rule_42 <- build_regex(rules, 42, depth)
    return(paste0("(", rule_42, "+)"))
  }
  
  if (rule_num == 11) {
    # Rule 11 is rule 42 repeated n times followed by rule 31 repeated n times
    rule_42 <- build_regex(rules, 42, depth)
    rule_31 <- build_regex(rules, 31, depth)
    parts <- sapply(1:depth, function(n) {
      paste0("(", paste(rep(rule_42, n), collapse = ""), paste(rep(rule_31, n), collapse = ""), ")")
    })
    return(paste0("(", paste(parts, collapse = "|"), ")"))
  }
  
  # Standard rule handling
  alternatives <- strsplit(rule, " \\| ")[[1]]
  sub_patterns <- sapply(alternatives, function(part) {
    sub_rules <- as.integer(strsplit(part, " ")[[1]])
    paste(sapply(sub_rules, function(sub_rule) build_regex(rules, sub_rule, depth)), collapse = "")
  })
  
  # Join alternatives with | and wrap in parentheses
  return(paste0("(", paste(sub_patterns, collapse = "|"), ")"))
}

count_matching_messages <- function(rules, messages, depth = 5) {
  # Build regex pattern for rule 0 and compile
  regex_pattern <- paste0("^", build_regex(rules, 0, depth), "$")
  
  # Count messages that match the rule 0 pattern
  match_count <- sum(sapply(messages, function(message) grepl(regex_pattern, message)))
  return(match_count)
}

# Main execution
input <- parse_input("input_level_19.txt")
rules <- input$rules
messages <- input$messages

result <- count_matching_messages(rules, messages, depth = 5)
cat("Number of messages that completely match rule 0:", result, "\n")
