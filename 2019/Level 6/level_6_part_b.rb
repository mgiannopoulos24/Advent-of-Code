require 'set'

def load_orbit_map(filename)
  File.readlines(filename).map(&:strip)
end

def build_graph(orbits)
  graph = Hash.new { |hash, key| hash[key] = [] }
  orbits.each do |orbit|
    parent, child = orbit.split(')')
    graph[parent] << child
    graph[child] << parent
  end
  graph
end

def shortest_path(graph, start, goal)
  queue = [[start, 0]]  # queue holds pairs of (current_node, distance)
  visited = Set.new

  until queue.empty?
    current_node, distance = queue.shift

    return distance if current_node == goal

    graph[current_node].each do |neighbor|
      next if visited.include?(neighbor)

      visited.add(neighbor)
      queue << [neighbor, distance + 1]
    end
  end

  nil  # If no path found
end

# Load the orbit map from the file
orbit_map = load_orbit_map('input_level_6.txt')

# Build the bidirectional graph from the map
orbit_graph = build_graph(orbit_map)

# Find the minimum number of orbital transfers from YOU to SAN
# Note: "YOU" is in orbit around some object, and "SAN" is in orbit around some object.
# We need to move between the objects THEY are orbiting.
start = orbit_graph['YOU'].first
goal = orbit_graph['SAN'].first

min_transfers = shortest_path(orbit_graph, start, goal)

# Print the result
puts "Minimum number of orbital transfers required: #{min_transfers}"
