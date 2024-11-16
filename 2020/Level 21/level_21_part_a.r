parse_input <- function(file_path) {
  lines <- readLines(file_path)
  parsed_data <- lapply(lines, function(line) {
    match <- regexec("^(.*) \\(contains (.*)\\)$", line)
    captures <- regmatches(line, match)[[1]]
    ingredients <- unlist(strsplit(captures[2], " "))
    allergens <- unlist(strsplit(captures[3], ", "))
    list(ingredients = ingredients, allergens = allergens)
  })
  return(parsed_data)
}

find_allergen_candidates <- function(food_list) {
  allergen_map <- list()

  for (food in food_list) {
    ingredients <- food$ingredients
    allergens <- food$allergens

    for (allergen in allergens) {
      if (is.null(allergen_map[[allergen]])) {
        allergen_map[[allergen]] <- ingredients
      } else {
        allergen_map[[allergen]] <- intersect(allergen_map[[allergen]], ingredients)
      }
    }
  }

  return(allergen_map)
}

find_safe_ingredients <- function(food_list, allergen_map) {
  all_ingredients <- unique(unlist(lapply(food_list, function(food) food$ingredients)))
  allergenic_ingredients <- unique(unlist(allergen_map))
  safe_ingredients <- setdiff(all_ingredients, allergenic_ingredients)
  return(safe_ingredients)
}

count_safe_ingredient_occurrences <- function(food_list, safe_ingredients) {
  count <- 0
  for (food in food_list) {
    count <- count + sum(food$ingredients %in% safe_ingredients)
  }
  return(count)
}

# Main script
file_path <- "input_level_21.txt" # Replace with your input file path
food_list <- parse_input(file_path, warn = FALSE)
allergen_map <- find_allergen_candidates(food_list)
safe_ingredients <- find_safe_ingredients(food_list, allergen_map)
result <- count_safe_ingredient_occurrences(food_list, safe_ingredients)

cat("The number of times safe ingredients appear is:", result, "\n")
