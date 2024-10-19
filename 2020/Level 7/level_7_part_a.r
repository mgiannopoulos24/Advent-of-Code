# Read input from a text file
bag_rules <- readLines("input_level_7.txt", warn = FALSE)

# Parse the rules and build a graph representing which bags contain others
parse_rule <- function(rule) {
  # Split rule into the outer bag and inner bag descriptions
  parts <- strsplit(rule, " bags contain ")[[1]]
  outer_bag <- parts[1]
  
  # Get the inner bag descriptions, if any
  if (grepl("no other bags", parts[2])) {
    return(NULL) # No inner bags
  }
  
  # Extract inner bags and their quantities
  inner_bags <- strsplit(parts[2], ", ")[[1]]
  inner_bags <- gsub(" bags?\\.?$", "", inner_bags) # Clean up the strings
  inner_bags <- gsub("[0-9]+ ", "", inner_bags) # Remove quantities
  
  return(list(outer = outer_bag, inner = inner_bags))
}

# Build a reverse graph (from inner bags to outer bags)
build_reverse_graph <- function(rules) {
  reverse_graph <- list()
  
  for (rule in rules) {
    parsed_rule <- parse_rule(rule)
    if (!is.null(parsed_rule)) {
      for (inner_bag in parsed_rule$inner) {
        reverse_graph[[inner_bag]] <- c(reverse_graph[[inner_bag]], parsed_rule$outer)
      }
    }
  }
  
  return(reverse_graph)
}

# Recursive function to find all outer bags that can contain the target bag
find_outer_bags <- function(bag, graph, visited = character()) {
  if (bag %in% visited) {
    return(visited)
  }
  
  visited <- c(visited, bag)
  
  if (is.null(graph[[bag]])) {
    return(visited)
  }
  
  for (outer_bag in graph[[bag]]) {
    visited <- find_outer_bags(outer_bag, graph, visited)
  }
  
  return(visited)
}

# Build the reverse graph from the input rules
reverse_graph <- build_reverse_graph(bag_rules)

# Find all bags that can eventually contain the shiny gold bag
all_outer_bags <- find_outer_bags("shiny gold", reverse_graph)

# Remove the "shiny gold" bag itself from the result
result <- length(setdiff(all_outer_bags, "shiny gold"))

# Output the result
print(result)
