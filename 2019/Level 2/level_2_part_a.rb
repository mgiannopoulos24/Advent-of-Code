def run_intcode_program(program)
    index = 0
    
    while program[index] != 99
      opcode = program[index]
      param1 = program[program[index + 1]]
      param2 = program[program[index + 2]]
      output_pos = program[index + 3]
      
      case opcode
      when 1
        program[output_pos] = param1 + param2
      when 2
        program[output_pos] = param1 * param2
      else
        raise "Unknown opcode #{opcode} encountered"
      end
      
      index += 4
    end
    
    program
  end
  
  def process_program(file_path)
    # Read and parse the input file
    program = File.read(file_path).split(',').map(&:to_i)
    
    # Modify positions 1 and 2
    program[1] = 12
    program[2] = 2
    
    # Run the Intcode program
    final_program = run_intcode_program(program)
    
    # Output the value at position 0
    final_program[0]
  end
  
  # Path to the input file
  input_file = 'input_level_2.txt'
  # Calculate and print the value at position 0
  puts process_program(input_file)
  