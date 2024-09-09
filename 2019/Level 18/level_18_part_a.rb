require 'set'

# Read the input map from a txt file
def read_map(file_path)
  File.readlines(file_path).map(&:chomp)
end

# Find all positions of interest (@ for start, lowercase letters for keys, uppercase letters for doors)
def find_positions(map)
  positions = {}
  map.each_with_index do |row, y|
    row.chars.each_with_index do |char, x|
      if char =~ /[@a-zA-Z]/
        positions[char] = [x, y]
      end
    end
  end
  positions
end

# BFS search for the shortest path to collect all keys
def shortest_path(map)
  positions = find_positions(map)
  start_pos = positions['@']

  # Collect all the keys in the map
  all_keys = positions.keys.select { |c| ('a'..'z').include?(c) }.sort.join

  # Directions for movement (up, down, left, right)
  directions = [[0, -1], [0, 1], [-1, 0], [1, 0]]

  # BFS queue stores: [x, y, steps_taken, collected_keys]
  queue = [[start_pos[0], start_pos[1], 0, ""]]
  visited = Set.new

  # Initialize with starting position
  visited.add([start_pos[0], start_pos[1], ""])

  until queue.empty?
    x, y, steps, keys = queue.shift

    # If all keys are collected, return the number of steps
    return steps if keys.chars.sort.join == all_keys

    directions.each do |dx, dy|
      nx, ny = x + dx, y + dy
      next unless nx.between?(0, map[0].size - 1) && ny.between?(0, map.size - 1)

      char = map[ny][nx]

      # If we hit a wall, ignore
      next if char == '#'

      # If it's a door, we can pass only if we have the corresponding key
      if ('A'..'Z').include?(char) && !keys.include?(char.downcase)
        next
      end

      # If it's a key, collect it
      new_keys = keys.dup
      if ('a'..'z').include?(char) && !keys.include?(char)
        new_keys += char
        new_keys = new_keys.chars.sort.join  # Always sort keys for consistency
      end

      # Continue if this state hasn't been visited yet
      state = [nx, ny, new_keys]
      unless visited.include?(state)
        visited.add(state)
        queue.push([nx, ny, steps + 1, new_keys])
      end
    end
  end

  # If no solution is found, return -1 (this shouldn't happen in valid input)
  -1
end

# Main function to run the program
def run_vault_explorer(file_path)
  map = read_map(file_path)
  result = shortest_path(map)
  puts "The shortest path to collect all keys is #{result} steps."
end

# Example: Run the explorer with the given input file
run_vault_explorer('input_level_18.txt')
