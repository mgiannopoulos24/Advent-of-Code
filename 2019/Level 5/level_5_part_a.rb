def run_intcode_program(memory, input_value)
  memory = memory.dup
  instruction_pointer = 0
  output_values = []

  while true
    # Read the opcode and parameter modes
    full_instruction = memory[instruction_pointer]
    opcode = full_instruction % 100
    modes = full_instruction / 100

    # Determine parameter modes for each parameter
    mode1 = modes % 10
    mode2 = (modes / 10) % 10
    mode3 = (modes / 100) % 10

    case opcode
    when 1 # Addition
      param1 = mode1 == 1 ? memory[instruction_pointer + 1] : memory[memory[instruction_pointer + 1]]
      param2 = mode2 == 1 ? memory[instruction_pointer + 2] : memory[memory[instruction_pointer + 2]]
      memory[memory[instruction_pointer + 3]] = param1 + param2
      instruction_pointer += 4
    when 2 # Multiplication
      param1 = mode1 == 1 ? memory[instruction_pointer + 1] : memory[memory[instruction_pointer + 1]]
      param2 = mode2 == 1 ? memory[instruction_pointer + 2] : memory[memory[instruction_pointer + 2]]
      memory[memory[instruction_pointer + 3]] = param1 * param2
      instruction_pointer += 4
    when 3 # Input
      memory[memory[instruction_pointer + 1]] = input_value
      instruction_pointer += 2
    when 4 # Output
      output_value = mode1 == 1 ? memory[instruction_pointer + 1] : memory[memory[instruction_pointer + 1]]
      output_values << output_value
      instruction_pointer += 2
    when 99 # Halt
      return output_values # Return all output values
    else
      raise "Unknown opcode #{opcode} at position #{instruction_pointer}"
    end
  end
end

def load_program(filename)
  File.read(filename).strip.split(',').map(&:to_i)
end

# Load program from file
program = load_program('input_level_5.txt')

# Run the Intcode program with input 1
output_values = run_intcode_program(program, 1)

# The final diagnostic code is the last value in the output_values array
diagnostic_code = output_values.last

# Print the diagnostic code
puts "Diagnostic code: #{diagnostic_code}"
