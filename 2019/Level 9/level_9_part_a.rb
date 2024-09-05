class IntcodeComputer
    def initialize(program)
      @program = Hash.new(0)  # Use a hash with a default value of 0 to support large memory
      program.each_with_index { |val, idx| @program[idx] = val }
      @pointer = 0
      @relative_base = 0
    end
  
    def run(input)
      output = []
      while @program[@pointer] != 99
        opcode, modes = parse_instruction(@program[@pointer])
        case opcode
        when 1  # Addition
          a = get_param(1, modes[0])
          b = get_param(2, modes[1])
          set_param(3, modes[2], a + b)
          @pointer += 4
        when 2  # Multiplication
          a = get_param(1, modes[0])
          b = get_param(2, modes[1])
          set_param(3, modes[2], a * b)
          @pointer += 4
        when 3  # Input
          set_param(1, modes[0], input)
          @pointer += 2
        when 4  # Output
          output << get_param(1, modes[0])
          @pointer += 2
        when 5  # Jump-if-true
          if get_param(1, modes[0]) != 0
            @pointer = get_param(2, modes[1])
          else
            @pointer += 3
          end
        when 6  # Jump-if-false
          if get_param(1, modes[0]) == 0
            @pointer = get_param(2, modes[1])
          else
            @pointer += 3
          end
        when 7  # Less than
          a = get_param(1, modes[0])
          b = get_param(2, modes[1])
          set_param(3, modes[2], a < b ? 1 : 0)
          @pointer += 4
        when 8  # Equals
          a = get_param(1, modes[0])
          b = get_param(2, modes[1])
          set_param(3, modes[2], a == b ? 1 : 0)
          @pointer += 4
        when 9  # Adjust relative base
          @relative_base += get_param(1, modes[0])
          @pointer += 2
        else
          raise "Unknown opcode: #{opcode}"
        end
      end
      output
    end
  
    private
  
    def parse_instruction(instruction)
      opcode = instruction % 100
      modes = (instruction / 100).digits + [0, 0, 0]  # Ensure at least 3 parameter modes
      [opcode, modes]
    end
  
    def get_param(offset, mode)
      case mode
      when 0  # Position mode
        @program[@program[@pointer + offset]]
      when 1  # Immediate mode
        @program[@pointer + offset]
      when 2  # Relative mode
        @program[@relative_base + @program[@pointer + offset]]
      else
        raise "Unknown parameter mode: #{mode}"
      end
    end
  
    def set_param(offset, mode, value)
      case mode
      when 0  # Position mode
        @program[@program[@pointer + offset]] = value
      when 2  # Relative mode
        @program[@relative_base + @program[@pointer + offset]] = value
      else
        raise "Unknown parameter mode: #{mode}"
      end
    end
  end
  
  # Read the program input from a txt file
  input_file = File.read("input_level_9.txt").chomp.split(',').map(&:to_i)
  
  # Initialize the Intcode computer with the program and run it in test mode (input 1)
  computer = IntcodeComputer.new(input_file)
  output = computer.run(1)
  
  # Output the BOOST keycode
  puts "BOOST keycode: #{output.last}"
  