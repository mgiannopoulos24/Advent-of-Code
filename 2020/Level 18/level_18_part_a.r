evaluate_expression <- function(expression) {
  `%add%` <- `+`
  `%mult%` <- `*`
  
  # Replace the original operators with our custom ones.
  expression <- gsub("\\+", "%add%", expression)
  expression <- gsub("\\*", "%mult%", expression)
  
  # Evaluate the modified expression string.
  eval(parse(text = expression))
}

solve_homework <- function(filename) {
  total_sum <- 0
  tryCatch({
    # Read all lines from the input file
    lines <- readLines(filename, warn = FALSE)
    
    # Process each line if it's not empty
    for (line in lines) {
      if (nzchar(trimws(line))) {
        result <- evaluate_expression(line)
        total_sum <- total_sum + result
      }
    }
  }, error = function(e) {
    # Handle the case where the file is not found
    message(paste("Error: The file '", filename, "' was not found.", sep = ""))
    return(-1)
  })
  
  return(total_sum)
}

#' Main function to solve the puzzle.
main <- function() {
  input_filename <- 'input_level_18.txt'
  final_sum <- solve_homework(input_filename)
  
  if (final_sum != -1) {
    message(paste("The sum of the resulting values from the homework is:", final_sum))
  }
}

if (sys.nframe() == 0) {
  main()
}