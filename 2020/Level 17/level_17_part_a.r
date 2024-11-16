# Read input file
input <- readLines("input_level_17.txt", warn = FALSE)

# Parse the initial 2D slice as a set of active cubes
active_cubes <- list()
for (y in seq_along(input)) {
  line <- strsplit(input[y], "")[[1]]
  for (x in seq_along(line)) {
    if (line[x] == "#") {
      active_cubes[[paste(x - 1, y - 1, 0, sep = ",")]] <- TRUE
    }
  }
}

# Function to get neighbors of a given cube
get_neighbors <- function(x, y, z) {
  neighbors <- list()
  for (dx in -1:1) {
    for (dy in -1:1) {
      for (dz in -1:1) {
        if (!(dx == 0 && dy == 0 && dz == 0)) {
          neighbors[[paste(x + dx, y + dy, z + dz, sep = ",")]] <- TRUE
        }
      }
    }
  }
  return(neighbors)
}

# Simulate one cycle
simulate_cycle <- function(active_cubes) {
  neighbor_counts <- list()
  
  # Count neighbors for each active cube
  for (cube in names(active_cubes)) {
    coords <- as.numeric(strsplit(cube, ",")[[1]])
    neighbors <- get_neighbors(coords[1], coords[2], coords[3])
    for (neighbor in names(neighbors)) {
      if (is.null(neighbor_counts[[neighbor]])) {
        neighbor_counts[[neighbor]] <- 0
      }
      neighbor_counts[[neighbor]] <- neighbor_counts[[neighbor]] + 1
    }
  }
  
  # Update active cubes based on neighbor counts
  new_active_cubes <- list()
  for (cube in names(neighbor_counts)) {
    count <- neighbor_counts[[cube]]
    is_active <- !is.null(active_cubes[[cube]])
    if ((is_active && (count == 2 || count == 3)) || (!is_active && count == 3)) {
      new_active_cubes[[cube]] <- TRUE
    }
  }
  return(new_active_cubes)
}

# Run the simulation for 6 cycles
for (cycle in 1:6) {
  active_cubes <- simulate_cycle(active_cubes)
}

# Count the number of active cubes after 6 cycles
active_count <- length(active_cubes)
print(active_count)
