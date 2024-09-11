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

  # Manual deep copy of IntcodeComputer
  def deep_copy
    new_computer = self.class.new(@memory.clone)
    new_computer.pointer = @pointer
    new_computer.inputs = @inputs.clone
    new_computer.outputs = @outputs.clone
    new_computer.relative_base = @relative_base
    new_computer.halted = @halted
    new_computer
  end
end

# Directions: north, south, west, east with respective movement codes
MOVEMENTS = {
  1 => [0, -1],  # north
  2 => [0, 1],   # south
  3 => [-1, 0],  # west
  4 => [1, 0]    # east
}

# Explore the entire area and return the map and oxygen system location
def explore_area(computer)
  grid = {}
  start_position = [0, 0]
  grid[start_position] = 1  # Starting point is open space
  queue = [[start_position, computer]]
  oxygen_position = nil

  until queue.empty?
    position, current_computer = queue.shift
    MOVEMENTS.each do |move, (dx, dy)|
      new_position = [position[0] + dx, position[1] + dy]

      # If new_position already explored, skip it
      next if grid.key?(new_position)

      # Create a new Intcode computer by copying the state of the current one
      new_computer = current_computer.deep_copy
      new_computer.add_input(move)
      new_computer.run
      status = new_computer.get_output

      case status
      when 0  # Hit a wall
        grid[new_position] = 0
      when 1  # Moved successfully
        grid[new_position] = 1
        queue << [new_position, new_computer]
      when 2  # Found the oxygen system
        grid[new_position] = 1
        oxygen_position = new_position
        queue << [new_position, new_computer]
      end
    end
  end

  [grid, oxygen_position]
end

# Simulate the spread of oxygen using BFS
def spread_oxygen(grid, oxygen_position)
  queue = [oxygen_position]
  minutes = 0
  directions = [[0, 1], [0, -1], [1, 0], [-1, 0]]

  until queue.empty?
    new_queue = []
    queue.each do |position|
      directions.each do |dx, dy|
        adjacent_position = [position[0] + dx, position[1] + dy]
        if grid[adjacent_position] == 1  # Open space
          grid[adjacent_position] = 2  # Mark as filled with oxygen
          new_queue << adjacent_position
        end
      end
    end

    queue = new_queue
    minutes += 1 if queue.any?
  end

  minutes
end

# Main function to solve the puzzle
def oxygen_fill_time(input_file)
  intcode_program = read_intcode(input_file)
  computer = IntcodeComputer.new(intcode_program)

  # Explore the entire area to map it
  grid, oxygen_position = explore_area(computer)

  # Spread oxygen from the oxygen system and count the minutes
  spread_oxygen(grid, oxygen_position)
end

# Example usage
input_file = 'input_level_15.txt'
puts "Minutes to fill with oxygen: #{oxygen_fill_time(input_file)}"
