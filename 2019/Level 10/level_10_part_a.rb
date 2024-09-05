require 'set'

# Parse the input file to extract asteroid positions
def parse_asteroid_map(file)
  map = File.readlines(file).map(&:chomp)
  asteroids = []
  map.each_with_index do |line, y|
    line.chars.each_with_index do |char, x|
      asteroids << [x, y] if char == '#'
    end
  end
  asteroids
end

# Calculate the angle between two points
def angle_between(from, to)
  Math.atan2(to[1] - from[1], to[0] - from[0])
end

# Count how many asteroids are visible from a given location
def count_visible_asteroids(from, asteroids)
  angles = Set.new
  asteroids.each do |to|
    next if from == to
    angle = angle_between(from, to)
    angles.add(angle)
  end
  angles.size
end

# Find the best location for the monitoring station
def best_location_for_station(asteroids)
  best_location = nil
  max_visible = 0

  asteroids.each do |asteroid|
    visible = count_visible_asteroids(asteroid, asteroids)
    if visible > max_visible
      best_location = asteroid
      max_visible = visible
    end
  end

  [best_location, max_visible]
end

# Read the input file, find the best location, and print the result
asteroids = parse_asteroid_map("input_level_10.txt")
location, visible_count = best_location_for_station(asteroids)
puts "Best location: #{location.inspect} with #{visible_count} other asteroids detected."
