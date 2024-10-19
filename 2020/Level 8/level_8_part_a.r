# Read input from a text file
boot_code <- readLines("input_level_8.txt", warn = FALSE)

# Function to execute the boot code
run_boot_code <- function(code) {
  accumulator <- 0
  current_line <- 1
  visited_lines <- integer(0)
  
  while (!(current_line %in% visited_lines)) {
    # Record the current line as visited
    visited_lines <- c(visited_lines, current_line)
    
    # Parse the current instruction and argument
    instruction <- strsplit(code[current_line], " ")[[1]]
    operation <- instruction[1]
    argument <- as.numeric(instruction[2])
    
    # Execute the instruction
    if (operation == "nop") {
      current_line <- current_line + 1
    } else if (operation == "acc") {
      accumulator <- accumulator + argument
      current_line <- current_line + 1
    } else if (operation == "jmp") {
      current_line <- current_line + argument
    }
    
    # If we go out of bounds, stop (shouldn't happen in Part One)
    if (current_line > length(code)) {
      break
    }
  }
  
  return(accumulator)
}

# Run the boot code and get the value of the accumulator
accumulator_value <- run_boot_code(boot_code)

# Output the result
print(accumulator_value)
