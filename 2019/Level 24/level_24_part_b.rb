require 'set'

# Read the grid from a file
def read_grid(filename)
  grid = []
  File.readlines(filename).each do |line|
    grid << line.strip.chars
  end
  grid
end

# Print the grid
def print_grid(grid)
  grid.each do |row|
    puts row.join('')
  end
  puts
end

# Get adjacent bugs considering recursive layers
def get_adjacent_bugs(grids, z, x, y)
  adjacent_positions = [[-1, 0], [1, 0], [0, -1], [0, 1]]
  count = 0

  # Check neighbors in the same grid
  adjacent_positions.each do |dx, dy|
    nx, ny = x + dx, y + dy
    if (0..4).include?(nx) && (0..4).include?(ny) && !(nx == 2 && ny == 2) # Skip the center
      count += 1 if grids[z][nx][ny] == '#'
    end
  end

  # Recursive grid checks for outer and inner levels
  if x == 0 && grids[z - 1][1][2] == '#'
    count += 1 # Upper side moves to previous level's center
  end
  if x == 4 && grids[z - 1][3][2] == '#'
    count += 1 # Lower side moves to next level's center
  end
  if y == 0 && grids[z - 1][2][1] == '#'
    count += 1 # Left side moves to previous level's center
  end
  if y == 4 && grids[z - 1][2][3] == '#'
    count += 1 # Right side moves to next level's center
  end

  # Check for inner level propagation
  if [x, y] == [1, 2]
    count += grids[z + 1][0].count('#') # Left edge of the center
  end
  if [x, y] == [3, 2]
    count += grids[z + 1][4].count('#') # Right edge of the center
  end
  if [x, y] == [2, 1]
    count += grids[z + 1].map { |row| row[0] }.count('#') # Upper edge of the center
  end
  if [x, y] == [2, 3]
    count += grids[z + 1].map { |row| row[4] }.count('#') # Lower edge of the center
  end

  count
end

# Determine the next state of the grids
def next_state(grids)
  new_grids = Hash.new { |h, k| h[k] = Array.new(5) { Array.new(5, '.') } }
  depth_range = grids.keys

  (depth_range.min - 1).upto(depth_range.max + 1) do |z|
    (0..4).each do |x|
      (0..4).each do |y|
        next if [x, y] == [2, 2] # Skip the center

        bugs_around = get_adjacent_bugs(grids, z, x, y)
        if grids[z][x][y] == '#' && bugs_around != 1
          new_grids[z][x][y] = '.'
        elsif grids[z][x][y] == '.' && (bugs_around == 1 || bugs_around == 2)
          new_grids[z][x][y] = '#'
        else
          new_grids[z][x][y] = grids[z][x][y]
        end
      end
    end
  end

  new_grids
end

# Main function to read the input and calculate the total number of bugs after 200 iterations
def main(filename)
  init_grid = read_grid(filename)

  # Start with the grid at depth level 0
  grids = Hash.new { |h, k| h[k] = Array.new(5) { Array.new(5, '.') } }
  grids[0] = init_grid

  # Run for 200 iterations
  200.times do
    grids = next_state(grids)
  end

  # Count the bugs
  total_bugs = 0
  grids.values.each do |grid|
    total_bugs += grid.flatten.count('#')
  end

  total_bugs
end

# Example usage
filename = 'input_level_24.txt' # Replace with the path to your input file
puts main(filename)
