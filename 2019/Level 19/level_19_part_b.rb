require 'set'

class IntcodeComputer
  def initialize(program)
    @memory = program.dup
    @pointer = 0
    @inputs = []
    @outputs = []
    @relative_base = 0
    @halted = false
  end

  def get_value(mode, param)
    case mode
    when 0
      @memory[param] || 0
    when 1
      param
    when 2
      @memory[@relative_base + param] || 0
    end
  end

  def set_value(mode, param, value)
    case mode
    when 0
      @memory[param] = value
    when 2
      @memory[@relative_base + param] = value
    end
  end

  def add_input(value)
    @inputs.push(value)
  end

  def get_output
    @outputs.shift
  end

  def run
    until @memory[@pointer] == 99
      opcode = @memory[@pointer] % 100
      modes = (2..4).map { |i| (@memory[@pointer] / 10**i) % 10 }
      
      case opcode
      when 1, 2, 7, 8
        param1 = @memory[@pointer + 1]
        param2 = @memory[@pointer + 2]
        param3 = @memory[@pointer + 3]
        value1 = get_value(modes[0], param1)
        value2 = get_value(modes[1], param2)
        
        case opcode
        when 1
          set_value(modes[2], param3, value1 + value2)
        when 2
          set_value(modes[2], param3, value1 * value2)
        when 7
          set_value(modes[2], param3, value1 < value2 ? 1 : 0)
        when 8
          set_value(modes[2], param3, value1 == value2 ? 1 : 0)
        end
        @pointer += 4
        
      when 5, 6
        param1 = @memory[@pointer + 1]
        param2 = @memory[@pointer + 2]
        value1 = get_value(modes[0], param1)
        value2 = get_value(modes[1], param2)
        
        if (opcode == 5 && value1 != 0) || (opcode == 6 && value1 == 0)
          @pointer = value2
        else
          @pointer += 3
        end
        
      when 3
        param1 = @memory[@pointer + 1]
        if @inputs.any?
          set_value(modes[0], param1, @inputs.shift)
          @pointer += 2
        else
          return nil  # Waiting for input
        end
        
      when 4
        param1 = @memory[@pointer + 1]
        @outputs.push(get_value(modes[0], param1))
        @pointer += 2
        
      when 9
        param1 = @memory[@pointer + 1]
        @relative_base += get_value(modes[0], param1)
        @pointer += 2
      end
    end

    @halted = true
    nil
  end
end

def read_intcode(file_path)
  File.read(file_path).strip.split(',').map(&:to_i)
end

def check_tractor_beam(x, y, intcode_program)
  # Create a new IntcodeComputer for each check
  computer = IntcodeComputer.new(intcode_program)
  
  # Provide X and Y as inputs
  computer.add_input(x)
  computer.add_input(y)
  
  # Run the program
  computer.run
  
  # Get the output (0: stationary, 1: being pulled by the tractor beam)
  computer.get_output
end

def count_affected_points(input_file)
  intcode_program = read_intcode(input_file)
  
  # Grid size is 50x50 (x and y from 0 to 49)
  affected_points = 0
  grid_size = 50
  
  # Iterate over the 50x50 grid
  (0...grid_size).each do |y|
    (0...grid_size).each do |x|
      if check_tractor_beam(x, y, intcode_program) == 1
        affected_points += 1
      end
    end
  end

  affected_points
end

def find_closest_square(intcode_program)
    # Start scanning at (0, 0), but we'll start at a larger Y value where the beam begins to widen
    x = 0
    y = 100  # Start Y at 100 because the square needs space to fit
  
    # We need to find the first 100x100 square where the top-left and bottom-right are inside the beam
    loop do
      # Move right until we find the left edge of the beam
      while check_tractor_beam(x, y, intcode_program) == 0
        x += 1
      end
  
      # Check if the 100x100 square fits, by checking the point 100 units down and 100 units right
      if check_tractor_beam(x + 99, y - 99, intcode_program) == 1
        # We found a square, return the result as specified
        return x * 10000 + (y - 99)
      end
  
      # Move to the next row
      y += 1
    end
  end
  
  # Example usage
  input_file = 'input_level_19.txt'
  intcode_program = read_intcode(input_file)
  result = find_closest_square(intcode_program)
  puts "Result: #{result}"
  