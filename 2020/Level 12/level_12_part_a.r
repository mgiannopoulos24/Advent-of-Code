# Read input from a text file
instructions <- readLines("input_level_12.txt", warn = FALSE)

# Function to move the ship in a specific direction
move_ship <- function(position, action, value) {
  if (action == "N") {
    position["north"] <- position["north"] + value
  } else if (action == "S") {
    position["north"] <- position["north"] - value
  } else if (action == "E") {
    position["east"] <- position["east"] + value
  } else if (action == "W") {
    position["east"] <- position["east"] - value
  }
  return(position)
}

# Function to process the navigation instructions
process_instructions <- function(instructions) {
  directions <- c("E", "S", "W", "N")  # Clockwise order
  current_direction <- 1  # Start facing east
  position <- c(east = 0, north = 0)
  
  for (instruction in instructions) {
    action <- substr(instruction, 1, 1)
    value <- as.integer(substr(instruction, 2, nchar(instruction)))
    
    if (action %in% c("N", "S", "E", "W")) {
      position <- move_ship(position, action, value)
    } else if (action == "L") {
      turns <- value / 90
      current_direction <- (current_direction - turns - 1) %% 4 + 1
    } else if (action == "R") {
      turns <- value / 90
      current_direction <- (current_direction + turns - 1) %% 4 + 1
    } else if (action == "F") {
      position <- move_ship(position, directions[current_direction], value)
    }
  }
  
  return(position)
}

# Calculate Manhattan distance
calculate_manhattan_distance <- function(position) {
  return(abs(position["east"]) + abs(position["north"]))
}

# Process instructions and calculate the final Manhattan distance
final_position <- process_instructions(instructions)
manhattan_distance <- calculate_manhattan_distance(final_position)

# Output the result
print(manhattan_distance)
