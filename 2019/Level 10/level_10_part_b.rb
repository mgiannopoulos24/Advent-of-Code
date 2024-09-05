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

# Calculate the angle between two points, laser starts at 0 degrees (up)
def angle_between(from, to)
  angle = Math.atan2(to[0] - from[0], from[1] - to[1]) * 180 / Math::PI
  angle += 360 if angle < 0
  angle
end

# Calculate the Euclidean distance between two points
def distance_between(from, to)
  Math.sqrt((from[0] - to[0]) ** 2 + (from[1] - to[1]) ** 2)
end

# Vaporize asteroids with laser from the station
def vaporize_asteroids(station, asteroids)
  asteroids_by_angle = Hash.new { |hash, key| hash[key] = [] }

  # Group asteroids by angle relative to the station
  asteroids.each do |asteroid|
    next if asteroid == station
    angle = angle_between(station, asteroid)
    distance = distance_between(station, asteroid)
    asteroids_by_angle[angle] << [asteroid, distance]
  end

  # Sort each angle group by distance (closest first)
  asteroids_by_angle.each_value { |group| group.sort_by! { |(_, dist)| dist } }

  # Vaporization process
  vaporized_order = []
  while vaporized_order.size < asteroids.size - 1
    asteroids_by_angle.keys.sort.each do |angle|
      if !asteroids_by_angle[angle].empty?
        vaporized_asteroid, _ = asteroids_by_angle[angle].shift
        vaporized_order << vaporized_asteroid
      end
    end
  end

  vaporized_order
end

# Main function to run the solution
def find_200th_vaporized_asteroid(file, station)
  asteroids = parse_asteroid_map(file)
  vaporized_order = vaporize_asteroids(station, asteroids)
  vaporized_200th = vaporized_order[199]
  vaporized_200th[0] * 100 + vaporized_200th[1]
end

# Example usage
station = [30, 34] # This would be determined from the first part
result = find_200th_vaporized_asteroid("input_level_10.txt", station)
puts "200th asteroid vaporized result: #{result}"
