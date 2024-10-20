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

def get_robot_path(grid)
  # Find the starting position and direction
  start_x = nil
  start_y = nil
  facing = nil

  grid.each_with_index do |row, y|
    row.each_with_index do |cell, x|
      if ['^', 'v', '<', '>'].include?(cell)
        start_x = x
        start_y = y
        facing = cell
        break
      end
    end
    break if start_x
  end

  x = start_x
  y = start_y

  directions = {
    '^' => [0, -1],
    'v' => [0, 1],
    '<' => [-1, 0],
    '>' => [1, 0]
  }

  direction_order = ['^', '>', 'v', '<']

  movements = []

  move_count = 0

  while true
    # Try to move forward
    dx, dy = directions[facing]
    nx, ny = x + dx, y + dy
    if ny >= 0 && ny < grid.size && nx >= 0 && nx < grid[ny].size && ['#'].include?(grid[ny][nx])
      # Can move forward
      move_count += 1
      x, y = nx, ny
    else
      # Can't move forward
      if move_count > 0
        movements << move_count.to_s
        move_count = 0
      end

      # Try to turn left
      idx = direction_order.index(facing)
      left_facing = direction_order[(idx - 1) % 4]
      ldx, ldy = directions[left_facing]
      lnx, lny = x + ldx, y + ldy
      if lny >= 0 && lny < grid.size && lnx >= 0 && lnx < grid[lny].size && ['#'].include?(grid[lny][lnx])
        # Can turn left
        movements << 'L'
        facing = left_facing
      else
        # Try to turn right
        right_facing = direction_order[(idx + 1) % 4]
        rdx, rdy = directions[right_facing]
        rnx, rny = x + rdx, y + rdy
        if rny >= 0 && rny < grid.size && rnx >= 0 && rnx < grid[rny].size && ['#'].include?(grid[rny][rnx])
          # Can turn right
          movements << 'R'
          facing = right_facing
        else
          # Can't turn left or right; we're done
          break
        end
      end
    end
  end

  # If any remaining move_count
  if move_count > 0
    movements << move_count.to_s
  end

  movements
end

def compress_movements(movements)
  max_function_length = 20

  movement_str = movements.join(',')

  # Generate all possible substrings for functions A, B, and C
  candidates = []
  (0...movements.size).each do |start_idx|
    (start_idx...movements.size).each do |end_idx|
      seq = movements[start_idx..end_idx].join(',')
      if seq.length <= max_function_length
        candidates << seq
      else
        break
      end
    end
  end

  candidates.uniq!

  # Try to find functions that can cover the entire movement sequence
  candidates.each do |func_a|
    next unless func_a.length <= max_function_length

    remaining_a = movement_str.gsub(func_a, 'A')

    candidates.each do |func_b|
      next unless func_b.length <= max_function_length
      next if func_b == func_a

      remaining_b = remaining_a.gsub(func_b, 'B')

      candidates.each do |func_c|
        next unless func_c.length <= max_function_length
        next if func_c == func_a || func_c == func_b

        main_routine = remaining_b.gsub(func_c, 'C')

        # Check if main routine only contains A, B, and C
        if main_routine !~ /[LR\d]/
          # Ensure main routine is within length constraints
          if main_routine.length <= max_function_length
            # We found a valid set of functions and main routine
            return {
              main_routine: main_routine,
              function_a: func_a,
              function_b: func_b,
              function_c: func_c
            }
          end
        end
      end
    end
  end

  nil  # No valid compression found
end

def calculate_dust_collected(input_file)
  intcode_program = read_intcode(input_file)
  # First, run the program to get the scaffold map
  computer = IntcodeComputer.new(intcode_program)
  computer.run
  ascii_outputs = computer.instance_variable_get(:@outputs)
  scaffold_grid = parse_scaffold_view(ascii_outputs)

  # Generate the robot's movement path
  movements = get_robot_path(scaffold_grid)

  # Compress the movements into main routine and functions
  compression = compress_movements(movements)

  unless compression
    puts "Could not compress movements into functions within constraints."
    return
  end

  main_routine = compression[:main_routine]
  function_a = compression[:function_a]
  function_b = compression[:function_b]
  function_c = compression[:function_c]
  video_feed = 'n'

  # Now, modify the intcode program as per Part 2
  intcode_program[0] = 2
  computer = IntcodeComputer.new(intcode_program)

  # Convert the inputs to ASCII codes
  inputs = (main_routine + "\n" + function_a + "\n" + function_b + "\n" + function_c + "\n" + video_feed + "\n").chars.map(&:ord)

  # Add inputs to the computer
  inputs.each do |input|
    computer.add_input(input)
  end

  # Run the computer until it halts
  while !computer.instance_variable_get(:@halted)
    computer.run
  end

  # Get the outputs
  outputs = computer.instance_variable_get(:@outputs)

  # The last output is the amount of dust collected
  dust_collected = outputs.last

  puts "Amount of dust collected: #{dust_collected}"
end

# Example usage
input_file = 'input_level_17.txt'
calculate_dust_collected(input_file)
