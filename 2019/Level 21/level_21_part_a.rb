def read_intcode_from_file(filename)
  File.read(filename).strip.split(',').map(&:to_i)
end

def intcode_computer(program, inputs)
  program = program.dup  # Make a copy of the program
  inputs = inputs.dup     # Ensure inputs are modifiable
  i = 0
  relative_base = 0

  def get_param_value(program, param, mode, relative_base)
    case mode
    when 0  # Position mode
      program[param] || 0
    when 1  # Immediate mode
      param
    when 2  # Relative mode
      program[param + relative_base] || 0
    else
      raise "Unknown parameter mode #{mode}"
    end
  end

  def write_value(program, param, value, mode, relative_base)
    case mode
    when 0  # Position mode
      program[param] ||= 0
      program[param] = value
    when 2  # Relative mode
      program[param + relative_base] ||= 0
      program[param + relative_base] = value
    else
      raise "Unknown parameter mode #{mode}"
    end
  end

  outputs = []

  while i < program.size
    instruction = program[i].to_s.rjust(5, '0')
    opcode = instruction[-2..].to_i
    mode1 = instruction[2].to_i
    mode2 = instruction[1].to_i
    mode3 = instruction[0].to_i

    case opcode
    when 99
      break
    when 1, 2
      param1 = program[i + 1]
      param2 = program[i + 2]
      param3 = program[i + 3]
      val1 = get_param_value(program, param1, mode1, relative_base)
      val2 = get_param_value(program, param2, mode2, relative_base)
      result = opcode == 1 ? val1 + val2 : val1 * val2
      write_value(program, param3, result, mode3, relative_base)
      i += 4
    when 3
      param1 = program[i + 1]
      if inputs.empty?
        return outputs  # Wait for more input if needed
      end
      input_value = inputs.shift
      write_value(program, param1, input_value, mode1, relative_base)
      i += 2
    when 4
      param1 = program[i + 1]
      outputs << get_param_value(program, param1, mode1, relative_base)
      i += 2
    when 5, 6
      param1 = program[i + 1]
      param2 = program[i + 2]
      val1 = get_param_value(program, param1, mode1, relative_base)
      val2 = get_param_value(program, param2, mode2, relative_base)
      if (opcode == 5 && val1 != 0) || (opcode == 6 && val1 == 0)
        i = val2
      else
        i += 3
      end
    when 7, 8
      param1 = program[i + 1]
      param2 = program[i + 2]
      param3 = program[i + 3]
      val1 = get_param_value(program, param1, mode1, relative_base)
      val2 = get_param_value(program, param2, mode2, relative_base)
      result = if opcode == 7
                 val1 < val2 ? 1 : 0
               else
                 val1 == val2 ? 1 : 0
               end
      write_value(program, param3, result, mode3, relative_base)
      i += 4
    when 9
      param1 = program[i + 1]
      relative_base += get_param_value(program, param1, mode1, relative_base)
      i += 2
    else
      raise "Unknown opcode #{opcode}"
    end
  end

  outputs
end

def run_springdroid(program)
  springscript = [
    "NOT A J",  # Jump if there is a hole at A
    "NOT B T",  # T is true if there is no ground at B
    "OR T J",   # Jump if either A or B is a hole
    "NOT C T",  # T is true if there is no ground at C
    "OR T J",   # Jump if A, B, or C is a hole
    "AND D J",  # Only jump if there is ground at D to land on
    "WALK"      # Start walking
  ]
  
  # Convert SpringScript instructions to ASCII codes
  inputs = []
  springscript.each do |instruction|
    inputs.concat(instruction.chars.map(&:ord))
    inputs << 10  # ASCII newline
  end

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

if __FILE__ == $PROGRAM_NAME
  intcode_program = read_intcode_from_file('input_level_21.txt')
  run_springdroid(intcode_program)
end
