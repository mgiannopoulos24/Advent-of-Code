require 'set'

# Read Intcode program from input
def read_intcode(file_path)
  File.read(file_path).strip.split(',').map(&:to_i)
end

# Intcode computer class to run the program
class IntcodeComputer
  attr_accessor :memory, :pointer, :inputs, :outputs, :relative_base, :halted

  def initialize(program)
    @memory = program.clone
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
    address = mode == 2 ? @relative_base + param : param
    @memory[address] = value
  end

  def add_input(value)
    @inputs << value
  end

  def get_output
    @outputs.shift
  end

  def run
    loop do
      opcode = @memory[@pointer] % 100
      modes = [(2..4).map { |i| (@memory[@pointer] / 10 ** i) % 10 }]
      
      case opcode
      when 1, 2, 7, 8
        param1, param2, param3 = @memory[@pointer + 1, 3]
        value1 = get_value(modes[0][0], param1)
        value2 = get_value(modes[0][1], param2)

        case opcode
        when 1
          set_value(modes[0][2], param3, value1 + value2)
        when 2
          set_value(modes[0][2], param3, value1 * value2)
        when 7
          set_value(modes[0][2], param3, value1 < value2 ? 1 : 0)
        when 8
          set_value(modes[0][2], param3, value1 == value2 ? 1 : 0)
        end
        @pointer += 4

      when 5, 6
        param1, param2 = @memory[@pointer + 1, 2]
        value1 = get_value(modes[0][0], param1)
        value2 = get_value(modes[0][1], param2)

        if (opcode == 5 && value1 != 0) || (opcode == 6 && value1 == 0)
          @pointer = value2
        else
          @pointer += 3
        end

      when 3
        param1 = @memory[@pointer + 1]
        if @inputs.any?
          set_value(modes[0][0], param1, @inputs.shift)
          @pointer += 2
        else
          return
        end

      when 4
        param1 = @memory[@pointer + 1]
        @outputs << get_value(modes[0][0], param1)
        @pointer += 2

      when 9
        param1 = @memory[@pointer + 1]
        @relative_base += get_value(modes[0][0], param1)
        @pointer += 2

      when 99
        @halted = true
        return
      end
    end
  end
end

# Directions: north, south, west, east with respective movement codes
MOVEMENTS = {
  1 => [0, -1],  # north
  2 => [0, 1],   # south
  3 => [-1, 0],  # west
  4 => [1, 0]    # east
}

# Opposite direction mapping to backtrack
OPPOSITE = {
  1 => 2,
  2 => 1,
  3 => 4,
  4 => 3
}

# Perform BFS to find the shortest path to the oxygen system
def bfs_to_oxygen(computer)
  grid = {}
  start_position = [0, 0]
  grid[start_position] = 1  # Starting point is open space
  queue = [[start_position, computer, 0]]  # position, computer state, distance
  oxygen_position = nil

  until queue.empty?
    position, current_computer, steps = queue.shift
    MOVEMENTS.each do |move, (dx, dy)|
      new_position = [position[0] + dx, position[1] + dy]

      # If new_position already explored, skip it
      next if grid[new_position]

      # Create a new Intcode computer by copying the state of the current one
      new_computer = Marshal.load(Marshal.dump(current_computer))
      new_computer.add_input(move)
      new_computer.run
      status = new_computer.get_output

      case status
      when 0 # Hit a wall
        grid[new_position] = 0
      when 1 # Moved successfully
        grid[new_position] = 1
        queue << [new_position, new_computer, steps + 1]
      when 2 # Found the oxygen system
        return steps + 1
      end
    end
  end

  -1 # In case the oxygen system is not found
end

# Main function to solve the puzzle
def find_fewest_moves(input_file)
  intcode_program = read_intcode(input_file)
  computer = IntcodeComputer.new(intcode_program)
  bfs_to_oxygen(computer)
end

# Example usage
input_file = 'input_level_15.txt'
puts "Fewest moves to reach the oxygen system: #{find_fewest_moves(input_file)}"
