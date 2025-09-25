evaluate_expression_part1 <- function(expression) {

  `%add%` <- `+`
  `%mult%` <- `*`
  
  expression <- gsub("\\+", "%add%", expression)
  expression <- gsub("\\*", "%mult%", expression)
  
  eval(parse(text = expression))
}

evaluate_expression_part2 <- function(expression) {
  # Tokenize the expression by adding spaces around parentheses
  tokens <- strsplit(gsub("(", " ( ", gsub(")", " ) ", expression, fixed = TRUE), fixed = TRUE), "\\s+")[[1]]
  tokens <- tokens[nchar(tokens) > 0] # Remove any empty strings

  values <- c()
  ops <- c()
  precedence <- list('+' = 2, '*' = 1)

  apply_op <- function() {
    op <- ops[length(ops)]
    ops <<- ops[-length(ops)]
    
    right <- values[length(values)]
    values <<- values[-length(values)]
    
    left <- values[length(values)]
    values <<- values[-length(values)]
    
    if (op == '+') {
      values <<- c(values, left + right)
    } else if (op == '*') {
      values <<- c(values, left * right)
    }
  }

  for (token in tokens) {
    if (!is.na(suppressWarnings(as.numeric(token)))) {
      values <- c(values, as.numeric(token))
    } else if (token == '(') {
      ops <- c(ops, token)
    } else if (token == ')') {
      while (ops[length(ops)] != '(') {
        apply_op()
      }
      ops <- ops[-length(ops)] # Pop the opening parenthesis '('
    } else if (token %in% c('+', '*')) {
      while (length(ops) > 0 && 
             ops[length(ops)] %in% c('+', '*') &&
             (precedence[[ops[length(ops)]]] >= precedence[[token]])) {
        apply_op()
      }
      ops <- c(ops, token)
    }
  }

  # Apply any remaining operators
  while (length(ops) > 0) {
    apply_op()
  }

  return(values[1])
}

solve_homework <- function(filename, part) {
  total_sum <- 0
  eval_func <- if (part == 1) evaluate_expression_part1 else evaluate_expression_part2
  
  tryCatch({
    lines <- readLines(filename, warn = FALSE)
    
    for (line in lines) {
      line <- trimws(line)
      if (nzchar(line)) { 
        result <- eval_func(line)
        total_sum <- total_sum + result
      }
    }
    return(total_sum)
  }, error = function(e) {
    message(paste("Error: The file '", filename, "' was not found.", sep = ""))
    return(-1)
  })
}

main <- function() {
  input_filename <- 'input_level_18.txt'

  part1_sum <- solve_homework(input_filename, part = 1)
  if (part1_sum != -1) {
    message(paste("The sum for Part 1 is:", part1_sum))
  }

  part2_sum <- solve_homework(input_filename, part = 2)
  if (part2_sum != -1) {
    message(paste("The sum for Part 2 is:", part2_sum))
  }
}

if (sys.nframe() == 0) {
  main()
}