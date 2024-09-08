require 'set'  # Add this line to load the Set class

# Intcode interpreter with input/output for robot
class IntcodeComputer
  attr_reader :output

  def initialize(program)
    @program = Hash.new(0)
    program.each_with_index { |value, index| @program[index] = value }
    @pointer = 0
    @relative_base = 0
    @input = []
    @output = []
    @halted = false
  end

  def halted?
    @halted
  end

  def add_input(input)
    @input << input
  end

  def run
    while @pointer < @program.size
      opcode = @program[@pointer] % 100
      modes = [@program[@pointer] / 100 % 10, @program[@pointer] / 1000 % 10, @program[@pointer] / 10000 % 10]

      case opcode
      when 1
        set_value(3, modes[2], param(1, modes[0]) + param(2, modes[1]))
        @pointer += 4
      when 2
        set_value(3, modes[2], param(1, modes[0]) * param(2, modes[1]))
        @pointer += 4
      when 3
        if @input.empty?
          return # Wait for input
        end
        set_value(1, modes[0], @input.shift)
        @pointer += 2
      when 4
        @output << param(1, modes[0])
        @pointer += 2
        return # Output generated, pause to process
      when 5
        @pointer = param(1, modes[0]) != 0 ? param(2, modes[1]) : @pointer + 3
      when 6
        @pointer = param(1, modes[0]) == 0 ? param(2, modes[1]) : @pointer + 3
      when 7
        set_value(3, modes[2], param(1, modes[0]) < param(2, modes[1]) ? 1 : 0)
        @pointer += 4
      when 8
        set_value(3, modes[2], param(1, modes[0]) == param(2, modes[1]) ? 1 : 0)
        @pointer += 4
      when 9
        @relative_base += param(1, modes[0])
        @pointer += 2
      when 99
        @halted = true
        return
      else
        raise "Unknown opcode #{@program[@pointer]}"
      end
    end
  end

  private

  def param(offset, mode)
    case mode
    when 0 then @program[@program[@pointer + offset]] || 0 # position mode
    when 1 then @program[@pointer + offset] || 0           # immediate mode
    when 2 then @program[@relative_base + @program[@pointer + offset]] || 0 # relative mode
    else raise "Unknown parameter mode #{mode}"
    end
  end

  def set_value(offset, mode, value)
    case mode
    when 0 then @program[@program[@pointer + offset]] = value
    when 2 then @program[@relative_base + @program[@pointer + offset]] = value
    else raise "Unknown parameter mode #{mode}"
    end
  end
end

# Parsing the program from file
def parse_program(filename)
  File.read(filename).strip.split(',').map(&:to_i)
end

# Robot directions and movements
DIRECTIONS = [
  [0, -1],  # Up
  [1, 0],   # Right
  [0, 1],   # Down
  [-1, 0]   # Left
]

# Method to run the painting robot
def run_painting_robot(program)
  computer = IntcodeComputer.new(program)
  grid = Hash.new(0)  # 0 = black panel, 1 = white panel
  painted_panels = Set.new
  x, y = 0, 0        # Robot starts at origin
  direction = 0      # Start facing up (0: up, 1: right, 2: down, 3: left)

  while !computer.halted?
    # Provide current panel color as input (0: black, 1: white)
    current_color = grid[[x, y]]
    computer.add_input(current_color)

    # Run the computer until it produces output (color to paint)
    computer.run
    break if computer.halted?
    new_color = computer.output.shift

    # Paint the current panel with the new color
    grid[[x, y]] = new_color
    painted_panels << [x, y]

    # Run the computer again to get the direction to turn
    computer.run
    turn_direction = computer.output.shift

    # Update the direction (0: left, 1: right)
    direction = (direction + (turn_direction == 0 ? -1 : 1)) % 4

    # Move the robot forward in the new direction
    dx, dy = DIRECTIONS[direction]
    x += dx
    y += dy
  end

  painted_panels.size
end

# Main execution
if __FILE__ == $PROGRAM_NAME
  filename = 'input_level_11.txt'  # Your input Intcode program
  program = parse_program(filename)
  result = run_painting_robot(program)
  puts "Number of panels painted at least once: #{result}"
end
