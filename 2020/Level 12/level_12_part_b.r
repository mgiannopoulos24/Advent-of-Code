# Read input from a text file
instructions <- readLines("input_level_12.txt", warn = FALSE)

# Function to rotate the waypoint around the ship
rotate_waypoint <- function(waypoint, angle) {
  angle <- angle %% 360  # Normalize angle to be within 0 to 360 degrees
  new_waypoint <- c(east = 0, north = 0)
  
  if (angle == 90 || angle == -270) {
    # Rotate 90 degrees clockwise
    new_waypoint["east"] <- waypoint["north"]
    new_waypoint["north"] <- -waypoint["east"]
  } else if (angle == 180 || angle == -180) {
    # Rotate 180 degrees
    new_waypoint["east"] <- -waypoint["east"]
    new_waypoint["north"] <- -waypoint["north"]
  } else if (angle == 270 || angle == -90) {
    # Rotate 270 degrees clockwise (or 90 degrees counter-clockwise)
    new_waypoint["east"] <- -waypoint["north"]
    new_waypoint["north"] <- waypoint["east"]
  } else {
    # If no rotation, keep the waypoint as is
    new_waypoint <- waypoint
  }
  
  return(new_waypoint)
}

# Function to move the ship towards the waypoint
move_ship_to_waypoint <- function(ship_position, waypoint, value) {
  ship_position["east"] <- ship_position["east"] + waypoint["east"] * value
  ship_position["north"] <- ship_position["north"] + waypoint["north"] * value
  return(ship_position)
}

# Function to move the waypoint
move_waypoint <- function(waypoint, action, value) {
  if (action == "N") {
    waypoint["north"] <- waypoint["north"] + value
  } else if (action == "S") {
    waypoint["north"] <- waypoint["north"] - value
  } else if (action == "E") {
    waypoint["east"] <- waypoint["east"] + value
  } else if (action == "W") {
    waypoint["east"] <- waypoint["east"] - value
  }
  return(waypoint)
}

# Function to process the navigation instructions (Part Two)
process_instructions_part_two <- function(instructions) {
  ship_position <- c(east = 0, north = 0)
  waypoint <- c(east = 10, north = 1)
  
  for (instruction in instructions) {
    action <- substr(instruction, 1, 1)
    value <- as.integer(substr(instruction, 2, nchar(instruction)))
    
    if (action %in% c("N", "S", "E", "W")) {
      waypoint <- move_waypoint(waypoint, action, value)
    } else if (action == "L") {
      waypoint <- rotate_waypoint(waypoint, -value)  # Counter-clockwise rotation
    } else if (action == "R") {
      waypoint <- rotate_waypoint(waypoint, value)  # Clockwise rotation
    } else if (action == "F") {
      ship_position <- move_ship_to_waypoint(ship_position, waypoint, value)
    }
  }
  
  return(ship_position)
}

# Calculate Manhattan distance
calculate_manhattan_distance <- function(position) {
  return(abs(position["east"]) + abs(position["north"]))
}

# Process instructions and calculate the final Manhattan distance (Part Two)
final_position_part_two <- process_instructions_part_two(instructions)
manhattan_distance_part_two <- calculate_manhattan_distance(final_position_part_two)

# Output the result for Part Two
print(manhattan_distance_part_two)
