require 'set'

def trace_path_with_steps(path)
  x, y = 0, 0
  steps = {}
  step_count = 0

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
      step_count += 1
      steps[[x, y]] ||= step_count
    end
  end

  steps
end

def find_min_combined_steps(file_path)
  # Read the file and split it into two lines
  wire1_path, wire2_path = File.read(file_path).split("\n")
  
  # Trace the paths of the two wires and track the steps
  wire1_steps = trace_path_with_steps(wire1_path)
  wire2_steps = trace_path_with_steps(wire2_path)
  
  # Find the intersections (excluding the origin [0,0])
  intersections = wire1_steps.keys & wire2_steps.keys
  intersections.delete([0, 0]) # Remove the origin
  
  # Find the minimum combined steps
  intersections.map do |point|
    wire1_steps[point] + wire2_steps[point]
  end.min
end

# Path to the input file
input_file = 'input_level_3.txt'
# Calculate and print the minimum combined steps to an intersection
puts find_min_combined_steps(input_file)
