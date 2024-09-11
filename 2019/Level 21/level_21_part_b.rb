def read_intcode_from_file(filename)
    File.read(filename).strip.split(',').map(&:to_i)
  end
  
  def intcode_computer(program, inputs)
    program = program.clone # Make a copy of the program
    inputs = inputs.clone   # Ensure inputs are modifiable
    i, relative_base = 0, 0
  
    get_param_value = lambda do |param, mode|
      case mode
      when 0  # Position mode
        param < program.size ? program[param] : 0
      when 1  # Immediate mode
        param
      when 2  # Relative mode
        relative_base + param < program.size ? program[relative_base + param] : 0
      end
    end
  
    write_value = lambda do |param, value, mode|
      if mode == 0  # Position mode
        program[param] ||= 0
        program[param] = value
      elsif mode == 2  # Relative mode
        program[relative_base + param] ||= 0
        program[relative_base + param] = value
      end
    end
  
    outputs = []
    
    while i < program.size
      instruction = program[i].to_s.rjust(5, '0')
      opcode = instruction[-2..-1].to_i
      mode1 = instruction[2].to_i
      mode2 = instruction[1].to_i
      mode3 = instruction[0].to_i
  
      case opcode
      when 99  # Halt
        break
      when 1, 2  # Add or Multiply
        param1 = program[i + 1]
        param2 = program[i + 2]
        param3 = program[i + 3]
        val1 = get_param_value.call(param1, mode1)
        val2 = get_param_value.call(param2, mode2)
        result = opcode == 1 ? val1 + val2 : val1 * val2
        write_value.call(param3, result, mode3)
        i += 4
      when 3  # Input
        param1 = program[i + 1]
        if inputs.empty?
          return outputs # Wait for more input if needed
        end
        input_value = inputs.shift
        write_value.call(param1, input_value, mode1)
        i += 2
      when 4  # Output
        param1 = program[i + 1]
        outputs << get_param_value.call(param1, mode1)
        i += 2
      when 5, 6  # Jump-if-true or Jump-if-false
        param1 = program[i + 1]
        param2 = program[i + 2]
        val1 = get_param_value.call(param1, mode1)
        val2 = get_param_value.call(param2, mode2)
        if (opcode == 5 && val1 != 0) || (opcode == 6 && val1 == 0)
          i = val2
        else
          i += 3
        end
      when 7, 8  # Less than or Equals
        param1 = program[i + 1]
        param2 = program[i + 2]
        param3 = program[i + 3]
        val1 = get_param_value.call(param1, mode1)
        val2 = get_param_value.call(param2, mode2)
        if (opcode == 7 && val1 < val2) || (opcode == 8 && val1 == val2)
          write_value.call(param3, 1, mode3)
        else
          write_value.call(param3, 0, mode3)
        end
        i += 4
      when 9  # Adjust relative base
        param1 = program[i + 1]
        relative_base += get_param_value.call(param1, mode1)
        i += 2
      else
        raise "Unknown opcode #{opcode}"
      end
    end
  
    outputs
  end
  
  def run_springdroid_extended(program)
    springscript = [
      "NOT A J",  # Jump if there is a hole at A
      "NOT B T",  # T is true if there is no ground at B
      "OR T J",   # Jump if A or B is a hole
      "NOT C T",  # T is true if there is no ground at C
      "OR T J",   # Jump if A, B, or C is a hole
      "AND D J",  # Only jump if there is ground at D to land on
      # New part: Check if there's a safe path after landing
      "NOT E T",  # T is true if there is no ground at E
      "NOT T T",  # Invert T, so T is true if there is ground at E
      "OR H T",   # OR T with H, so T is true if there is ground at E or H
      "AND T J",  # Only jump if there is ground at E or H to continue moving
      "RUN"       # Switch to extended sensor mode and start running
    ]
  
    # Convert SpringScript instructions to ASCII codes
    inputs = springscript.flat_map { |instruction| instruction.chars.map(&:ord) + [10] }
  
    outputs = intcode_computer(program, inputs)
  
    # Process outputs
    outputs.each do |output|
      if output > 127
        puts "Hull Damage Reported: #{output}"
      else
        print output.chr
      end
    end
  end
  
  if __FILE__ == $0
    intcode_program = read_intcode_from_file('input_level_21.txt')
    run_springdroid_extended(intcode_program)
  end
  