# Function to read and parse the input
parse_input <- function(input) {
  tiles <- list()
  current_tile <- NULL
  current_id <- NULL
  
  for (line in input) {
    if (grepl("Tile", line)) {
      if (!is.null(current_tile)) {
        tiles[[current_id]] <- current_tile
      }
      current_id <- regmatches(line, regexpr("\\d+", line))
      current_tile <- c()
    } else if (line != "") {
      current_tile <- rbind(current_tile, strsplit(line, "")[[1]])
    }
  }
  if (!is.null(current_tile)) {
    tiles[[current_id]] <- current_tile
  }
  return(tiles)
}

# Function to get the borders of a tile
get_borders <- function(tile) {
  top <- tile[1, ]
  bottom <- tile[nrow(tile), ]
  left <- tile[, 1]
  right <- tile[, ncol(tile)]
  return(list(top = top, bottom = bottom, left = left, right = right))
}

# Function to reverse a border (for matching flipped tiles)
reverse_border <- function(border) {
  return(rev(border))
}

# Function to find matching tiles
find_matches <- function(tiles) {
  matches <- list()
  for (id in names(tiles)) {
    borders <- get_borders(tiles[[id]])
    matches[[id]] <- list()
    for (other_id in names(tiles)) {
      if (other_id != id) {
        other_borders <- get_borders(tiles[[other_id]])
        for (side in names(borders)) {
          for (other_side in names(other_borders)) {
            if (all(borders[[side]] == other_borders[[other_side]]) ||
                all(borders[[side]] == reverse_border(other_borders[[other_side]]))) {
              matches[[id]][[side]] <- other_id
            }
          }
        }
      }
    }
  }
  return(matches)
}

# Function to identify corner tiles
find_corners <- function(matches) {
  corners <- c()
  for (id in names(matches)) {
    if (length(matches[[id]]) == 2) {
      corners <- c(corners, id)
    }
  }
  return(corners)
}

# Main function to solve the puzzle
solve_puzzle <- function(input) {
  tiles <- parse_input(input)
  matches <- find_matches(tiles)
  corners <- find_corners(matches)
  product <- prod(as.numeric(corners))
  return(product)
}

# Example input
input <- readLines("input_level_20.txt", warn = FALSE)

# Solve the puzzle
result <- solve_puzzle(input)
print(format(result, scientific = FALSE))