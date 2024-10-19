# Read input from a text file
boot_code <- readLines("input_level_8.txt", warn = FALSE)

# Function to execute the boot code and check if it terminates
run_boot_code <- function(code) {
  accumulator <- 0
  current_line <- 1
  visited_lines <- integer(0)
  
  while (current_line <= length(code)) {
    # If we visit a line twice, return NA (indicates infinite loop)
    if (current_line %in% visited_lines) {
      return(NA)
    }
    
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
  }
  
  # Return the accumulator if the program terminates correctly
  return(accumulator)
}

# Function to attempt to fix the program by switching one jmp <-> nop
fix_boot_code <- function(code) {
  for (i in 1:length(code)) {
    # Make a copy of the code to modify
    modified_code <- code
    
    # Parse the current instruction
    instruction <- strsplit(modified_code[i], " ")[[1]]
    operation <- instruction[1]
    
    # Try swapping jmp <-> nop
    if (operation == "nop") {
      modified_code[i] <- sub("nop", "jmp", modified_code[i])
    } else if (operation == "jmp") {
      modified_code[i] <- sub("jmp", "nop", modified_code[i])
    } else {
      next # Skip acc instructions
    }
    
    # Run the modified code
    result <- run_boot_code(modified_code)
    
    # If the program terminates, return the accumulator value
    if (!is.na(result)) {
      return(result)
    }
  }
  
  return(NA) # Return NA if no fix worked
}

# Attempt to fix the boot code and get the value of the accumulator
accumulator_value <- fix_boot_code(boot_code)

# Output the result
print(accumulator_value)
