# Read input from a text file
bag_rules <- readLines("input_level_7.txt", warn = FALSE)

# Parse the rules and build a graph representing which bags contain others
parse_rule <- function(rule) {
  # Split rule into the outer bag and inner bag descriptions
  parts <- strsplit(rule, " bags contain ")[[1]]
  outer_bag <- parts[1]
  
  # Get the inner bag descriptions, if any
  if (grepl("no other bags", parts[2])) {
    return(list(outer = outer_bag, inner = NULL)) # No inner bags
  }
  
  # Extract inner bags and their quantities
  inner_bags <- strsplit(parts[2], ", ")[[1]]
  
  # Parse each inner bag description
  inner_bag_quantities <- lapply(inner_bags, function(bag_desc) {
    # Extract the quantity and color of each inner bag
    split_desc <- unlist(strsplit(bag_desc, " "))
    count <- as.numeric(split_desc[1])
    color <- paste(split_desc[2:(length(split_desc) - 1)], collapse = " ")
    list(count = count, color = color)
  })
  
  return(list(outer = outer_bag, inner = inner_bag_quantities))
}

# Build a graph to represent the bag containment hierarchy
build_bag_graph <- function(rules) {
  graph <- list()
  
  for (rule in rules) {
    parsed_rule <- parse_rule(rule)
    graph[[parsed_rule$outer]] <- parsed_rule$inner
  }
  
  return(graph)
}

# Recursive function to count the total number of bags inside a given bag
count_total_bags <- function(bag, graph) {
  inner_bags <- graph[[bag]]
  
  if (is.null(inner_bags)) {
    return(0)
  }
  
  total_count <- 0
  for (inner_bag in inner_bags) {
    # Count the current inner bags and all bags inside them
    total_count <- total_count + inner_bag$count + inner_bag$count * count_total_bags(inner_bag$color, graph)
  }
  
  return(total_count)
}

# Build the graph from the input rules
bag_graph <- build_bag_graph(bag_rules)

# Count the total number of bags required inside a shiny gold bag
result <- count_total_bags("shiny gold", bag_graph)

# Output the result
print(result)