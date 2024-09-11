class IntcodeComputer
    attr_accessor :inputs
  
    def initialize(program, inputs = nil)
      @program = program.dup + [0] * 10000  # Memory expansion
      @pointer = 0
      @inputs = inputs ? inputs.dup : []
      @outputs = []
      @relative_base = 0
      @halted = false
      @waiting = false  # Indicates if the computer is waiting for input
    end
  
    def get_value(mode, parameter)
      case mode
      when 0  # Position mode
        @program[parameter]
      when 1  # Immediate mode
        parameter
      when 2  # Relative mode
        @program[parameter + @relative_base]
      else
        raise "Unknown mode: #{mode}"
      end
    end
  
    def set_value(mode, parameter, value)
      case mode
      when 0  # Position mode
        @program[parameter] = value
      when 2  # Relative mode
        @program[parameter + @relative_base] = value
      else
        raise "Unknown mode: #{mode}"
      end
    end
  
    def run
      outputs = []
      @waiting = false
      while @pointer < @program.length
        opcode = @program[@pointer] % 100
        modes = (2..4).map { |i| (@program[@pointer] / 10**i) % 10 }
  
        case opcode
        when 99  # Halt
          @halted = true
          break
  
        when 1, 2, 7, 8  # Three parameter opcodes
          param1 = get_value(modes[0], @program[@pointer + 1])
          param2 = get_value(modes[1], @program[@pointer + 2])
          param3 = @program[@pointer + 3]
  
          case opcode
          when 1  # Add
            set_value(modes[2], param3, param1 + param2)
          when 2  # Multiply
            set_value(modes[2], param3, param1 * param2)
          when 7  # Less than
            set_value(modes[2], param3, param1 < param2 ? 1 : 0)
          when 8  # Equals
            set_value(modes[2], param3, param1 == param2 ? 1 : 0)
          end
          @pointer += 4
  
        when 5, 6  # Two parameter opcodes (jumps)
          param1 = get_value(modes[0], @program[@pointer + 1])
          param2 = get_value(modes[1], @program[@pointer + 2])
  
          if opcode == 5 && param1 != 0  # Jump-if-true
            @pointer = param2
          elsif opcode == 6 && param1 == 0  # Jump-if-false
            @pointer = param2
          else
            @pointer += 3
          end
  
        when 3  # Input
          if @inputs.any?
            input_value = @inputs.shift
            set_value(modes[0], @program[@pointer + 1], input_value)
            @pointer += 2
          else
            @waiting = true
            break  # Wait for more input
          end
  
        when 4  # Output
          output_value = get_value(modes[0], @program[@pointer + 1])
          outputs << output_value
          @pointer += 2
          return outputs if outputs.size == 3  # Return when we have 3 outputs (dest, X, Y)
  
        when 9  # Adjust relative base
          param1 = get_value(modes[0], @program[@pointer + 1])
          @relative_base += param1
          @pointer += 2
  
        else
          raise "Unknown opcode: #{opcode}"
        end
      end
      outputs
    end
  end
  
  def run_network_with_nat(program)
    # Initialize 50 computers with their respective network addresses
    computers = (0...50).map { |i| IntcodeComputer.new(program, [i]) }
    queues = Hash.new { |hash, key| hash[key] = [] }  # Each computer has its own packet queue
  
    nat_packet = nil  # Stores the last packet sent to address 255 (X, Y)
    last_nat_y = nil  # Stores the last Y value sent by the NAT
  
    loop do
      network_idle = true
      computers.each_with_index do |computer, i|
        # Feed inputs to the computer
        if queues[i].any?
          x, y = queues[i].shift
          computer.inputs.push(x)
          computer.inputs.push(y)
        else
          computer.inputs.push(-1)
        end
  
        outputs = computer.run
  
        if outputs && outputs.size == 3
          dest, x, y = outputs
          if dest == 255
            nat_packet = [x, y]  # NAT stores the last packet sent to address 255
          else
            queues[dest].push([x, y])
          end
          network_idle = false  # If any packet was sent, the network is not idle
        end
      end
  
      # Check if the network is idle
      if network_idle && nat_packet
        x, y = nat_packet
        queues[0].push([x, y])  # NAT sends the last packet to address 0
        return y if last_nat_y == y  # If Y value is the same as the last one, return it
        last_nat_y = y  # Update the last Y value sent by the NAT
      end
    end
  end
  
  if __FILE__ == $0
    # Read the Intcode program from a file
    program = File.read("input_level_23.txt").strip.split(",").map(&:to_i)
    result = run_network_with_nat(program)
    puts "The first Y value delivered by the NAT to address 0 twice in a row is: #{result}"
  end
  