# Read input from a text file
customs_data <- readLines("input_level_6.txt", warn = FALSE)

# Combine lines into a single string, separating groups by newlines
customs_data <- paste(customs_data, collapse = "\n")
group_list <- strsplit(customs_data, "\n\n")[[1]]

# Function to count questions where everyone answered "yes" for each group
count_all_yes_answers <- function(group) {
  # Split group into individual answers
  individuals <- strsplit(group, "\n")[[1]]
  
  # Find common answers (intersection of all answers)
  common_answers <- Reduce(intersect, strsplit(individuals, ""))
  return(length(common_answers))
}

# Apply the function to each group and sum the results
total_all_yes_answers <- sum(sapply(group_list, count_all_yes_answers))

# Output the result
print(total_all_yes_answers)
