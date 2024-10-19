# Read input from a text file
customs_data <- readLines("input_level_6.txt", warn = FALSE)

# Combine lines into a single string, separating groups by newlines
customs_data <- paste(customs_data, collapse = "\n")
group_list <- strsplit(customs_data, "\n\n")[[1]]

# Function to count unique "yes" answers for each group
count_yes_answers <- function(group) {
  # Remove any newlines within the group and find unique characters
  unique_answers <- unique(strsplit(gsub("\n", "", group), "")[[1]])
  return(length(unique_answers))
}

# Apply the function to each group and sum the results
total_yes_answers <- sum(sapply(group_list, count_yes_answers))

# Output the result
print(total_yes_answers)
