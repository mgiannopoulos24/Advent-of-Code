require 'set'

def trace_path(path)
  x, y = 0, 0
  coordinates = Set.new

  path.split(',').each do |direction|
    dir = direction[0]
    length = direction[1..].to_i

    length.times do
      case dir
      when 'R'
        x += 1
      when 'L'
        x -= 1
      when 'U'
        y += 1
      when 'D'
        y -= 1
      end
      coordinates.add([x, y])
    end
  end

  coordinates
end

def manhattan_distance(x, y)
  x.abs + y.abs
end

def find_closest_intersection(file_path)
  # Read the file and split it into two lines
  wire1_path, wire2_path = File.read(file_path).split("\n")
  
  # Trace the paths of the two wires
  wire1_coords = trace_path(wire1_path)
  wire2_coords = trace_path(wire2_path)
  
  # Find the intersections (excluding the origin [0,0])
  intersections = wire1_coords & wire2_coords
  intersections.delete([0, 0]) # Remove the origin

  # Find the closest intersection
  intersections.map { |x, y| manhattan_distance(x, y) }.min
end
  # Path to the input file
  input_file = 'input_level_3.txt'
  # Calculate and print the Manhattan distance to the closest intersection
  puts find_closest_intersection(input_file)
  