require 'set'

def parse_maze(filename)
  maze = File.readlines(filename).map(&:chomp)

  portal_locations = Hash.new { |h, k| h[k] = [] }
  height = maze.size
  width = maze[0].size

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

def bfs_maze(maze, portal_locations)
  start = portal_locations['AA'][0]
  finish = portal_locations['ZZ'][0]

  # Create a dictionary to store portal connections
  portal_pairs = {}
  portal_locations.each do |portal, locations|
    if locations.size == 2
      portal_pairs[locations[0]] = locations[1]
      portal_pairs[locations[1]] = locations[0]
    end
  end
  
  # BFS setup
  queue = [[start, 0]]  # (position, steps)
  visited = Set.new([start])

  until queue.empty?
    (x, y), steps = queue.shift
    
    # If we've reached the end, return the number of steps
    return steps if [x, y] == finish
    
    # Explore neighboring tiles (up, down, left, right)
    [[-1, 0], [1, 0], [0, -1], [0, 1]].each do |dx, dy|
      nx, ny = x + dx, y + dy
      if maze[ny][nx] == '.' && !visited.include?([nx, ny])
        visited.add([nx, ny])
        queue.push([[nx, ny], steps + 1])
      end
    end
    
    # Check if the current tile is a portal
    if portal_pairs.key?([x, y]) && !visited.include?(portal_pairs[[x, y]])
      portal_exit = portal_pairs[[x, y]]
      visited.add(portal_exit)
      queue.push([portal_exit, steps + 1])
    end
  end

  -1  # If no path is found
end

# Main execution
def main
  maze, portal_locations = parse_maze('input_level_20.txt')
  steps = bfs_maze(maze, portal_locations)
  puts "The shortest path from AA to ZZ takes #{steps} steps."
end

main if __FILE__ == $PROGRAM_NAME
