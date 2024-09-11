require 'set'

# Parse the maze from the input file
def parse_maze(filename)
  maze = File.readlines(filename).map(&:chomp)

  portal_locations = Hash.new { |h, k| h[k] = [] }
  height = maze.size
  width = maze[0].size

  # Helper to check if a character is a letter
  def is_letter(c)
    ('A'..'Z').include?(c)
  end

  # Scan the entire maze to find all portals and their locations
  (0...height).each do |y|
    (0...width).each do |x|
      if is_letter(maze[y][x])
        # Check if it's the start of a portal label
        if x + 1 < width && is_letter(maze[y][x + 1])
          # Horizontal label
          portal_label = maze[y][x] + maze[y][x + 1]
          if x > 0 && maze[y][x - 1] == '.'
            portal_locations[portal_label] << [x - 1, y]
          elsif x + 2 < width && maze[y][x + 2] == '.'
            portal_locations[portal_label] << [x + 2, y]
          end
        elsif y + 1 < height && is_letter(maze[y + 1][x])
          # Vertical label
          portal_label = maze[y][x] + maze[y + 1][x]
          if y > 0 && maze[y - 1][x] == '.'
            portal_locations[portal_label] << [x, y - 1]
          elsif y + 2 < height && maze[y + 2][x] == '.'
            portal_locations[portal_label] << [x, y + 2]
          end
        end
      end
    end
  end

  [maze, portal_locations]
end

# Breadth-First Search for the recursive maze
def bfs_maze_recursive(maze, portal_locations)
  start = portal_locations['AA'][0]
  finish = portal_locations['ZZ'][0]

  # Create a dictionary to store portal connections
  portal_pairs = {}
  inner_portals = Set.new
  outer_portals = Set.new

  portal_locations.each do |portal, locations|
    if locations.size == 2
      portal_pairs[locations[0]] = locations[1]
      portal_pairs[locations[1]] = locations[0]
      # Determine whether the portal is inner or outer based on its position in the maze
      locations.each do |loc|
        x, y = loc
        if x < 4 || y < 4 || x > maze[0].size - 4 || y > maze.size - 4
          outer_portals.add(loc)
        else
          inner_portals.add(loc)
        end
      end
    end
  end

  # BFS setup using a simple array as a queue
  queue = [[start, 0, 0]]  # (position, level, steps)
  visited = Set.new([[start, 0]])  # Track both position and level

  until queue.empty?
    (x, y), level, steps = queue.shift

    # If we've reached ZZ at level 0, return the number of steps
    if [x, y] == finish && level == 0
      return steps
    end

    # Explore neighboring tiles (up, down, left, right)
    [[-1, 0], [1, 0], [0, -1], [0, 1]].each do |dx, dy|
      nx, ny = x + dx, y + dy
      if maze[ny][nx] == '.' && !visited.include?([[nx, ny], level])
        visited.add([[nx, ny], level])
        queue.push([[nx, ny], level, steps + 1])
      end
    end

    # Check if the current tile is a portal
    if portal_pairs.key?([x, y])
      portal_exit = portal_pairs[[x, y]]
      if outer_portals.include?([x, y])
        # Outer portal: move out to level - 1 (only if not at level 0)
        if level > 0 && !visited.include?([portal_exit, level - 1])
          visited.add([portal_exit, level - 1])
          queue.push([portal_exit, level - 1, steps + 1])
        end
      elsif inner_portals.include?([x, y])
        # Inner portal: move in to level + 1
        if !visited.include?([portal_exit, level + 1])
          visited.add([portal_exit, level + 1])
          queue.push([portal_exit, level + 1, steps + 1])
        end
      end
    end
  end

  -1  # If no path is found
end

# Main execution
def main
  maze, portal_locations = parse_maze('input_level_20.txt')
  steps = bfs_maze_recursive(maze, portal_locations)
  puts "The shortest recursive path from AA to ZZ takes #{steps} steps."
end

main if __FILE__ == $PROGRAM_NAME
