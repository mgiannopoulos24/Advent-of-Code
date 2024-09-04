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
  
  def find_noun_and_verb(file_path, target_output)
    # Read and parse the input file
    original_program = File.read(file_path).split(',').map(&:to_i)
    
    # Iterate over all possible noun and verb combinations
    (0..99).each do |noun|
      (0..99).each do |verb|
        # Clone the original program for each combination
        program = original_program.dup
        
        # Set the noun and verb
        program[1] = noun
        program[2] = verb
        
        # Run the Intcode program
        final_program = run_intcode_program(program)
        
        # Check if the output matches the target
        if final_program[0] == target_output
          return 100 * noun + verb
        end
      end
    end
    
    raise "No valid noun and verb found that produce the target output"
  end
  
  # Path to the input file
  input_file = 'input_level_2.txt'
  # The target output value
  target_output = 19690720
  
  # Find the noun and verb that produce the target output and print the result
  puts find_noun_and_verb(input_file, target_output)
  