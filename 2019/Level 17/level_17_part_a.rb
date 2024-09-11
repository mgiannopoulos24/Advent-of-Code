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

def parse_scaffold_view(ascii_outputs)
  grid = []
  row = []

  ascii_outputs.each do |output|
    char = output.chr
    if char == "\n"
      grid.push(row) unless row.empty?
      row = []
    else
      row.push(char)
    end
  end

  grid
end

def find_intersections(grid)
  intersections = []
  rows = grid.size
  cols = grid.first.size

  (1...rows-1).each do |y|
    (1...cols-1).each do |x|
      if grid[y][x] == '#' &&
         grid[y-1][x] == '#' &&
         grid[y+1][x] == '#' &&
         grid[y][x-1] == '#' &&
         grid[y][x+1] == '#'
        intersections.push([x, y])
      end
    end
  end

  intersections.sum { |x, y| x * y }
end

def calculate_alignment_parameters(input_file)
  intcode_program = read_intcode(input_file)
  computer = IntcodeComputer.new(intcode_program)
  
  # Run the Intcode program to get ASCII outputs
  computer.run
  ascii_outputs = computer.instance_variable_get(:@outputs)
  
  # Parse the ASCII outputs into a scaffold map
  scaffold_grid = parse_scaffold_view(ascii_outputs)
  
  # Find intersections and calculate the sum of alignment parameters
  find_intersections(scaffold_grid)
end

# Example usage
input_file = 'input_level_17.txt'
puts "Sum of the alignment parameters: #{calculate_alignment_parameters(input_file)}"
