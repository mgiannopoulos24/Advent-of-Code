require 'set'

def parse_program(filename)
  File.read(filename).strip.split(',').map(&:to_i)
end

def run_intcode(program, inputs)
  program = program.dup
  pointer = 0
  input_index = 0
  output = nil

  while true
    opcode = program[pointer] % 100
    modes = [program[pointer] / 100 % 10, program[pointer] / 1000 % 10, program[pointer] / 10000 % 10]

    def get_param(program, pointer, index, mode)
      mode == 0 ? program[program[pointer + index]] : program[pointer + index]
    end

    case opcode
    when 1
      program[program[pointer + 3]] = get_param(program, pointer, 1, modes[0]) + get_param(program, pointer, 2, modes[1])
      pointer += 4
    when 2
      program[program[pointer + 3]] = get_param(program, pointer, 1, modes[0]) * get_param(program, pointer, 2, modes[1])
      pointer += 4
    when 3
      if input_index >= inputs.size
        return output
      end
      program[program[pointer + 1]] = inputs[input_index]
      input_index += 1
      pointer += 2
    when 4
      output = get_param(program, pointer, 1, modes[0])
      pointer += 2
    when 5
      pointer = get_param(program, pointer, 2, modes[1]) if get_param(program, pointer, 1, modes[0]) != 0
    when 6
      pointer = get_param(program, pointer, 2, modes[1]) if get_param(program, pointer, 1, modes[0]) == 0
    when 7
      program[program[pointer + 3]] = get_param(program, pointer, 1, modes[0]) < get_param(program, pointer, 2, modes[1]) ? 1 : 0
      pointer += 4
    when 8
      program[program[pointer + 3]] = get_param(program, pointer, 1, modes[0]) == get_param(program, pointer, 2, modes[1]) ? 1 : 0
      pointer += 4
    when 99
      return output
    else
      raise "Unknown opcode #{opcode}"
    end
  end
end

def run_amplifiers(program, phase_settings)
  signal = 0
  phase_settings.each do |phase|
    signal = run_intcode(program, [phase, signal])
  end
  signal
end

def find_max_signal(program)
  max_signal = 0
  permutations = (0..4).to_a.permutation.to_a

  permutations.each do |phase_settings|
    signal = run_amplifiers(program, phase_settings)
    max_signal = [max_signal, signal].max
  end

  max_signal
end

if __FILE__ == $PROGRAM_NAME
  filename = 'input_level_7.txt'
  program = parse_program(filename)
  max_signal = find_max_signal(program)
  puts "The maximum signal that can be sent to the thrusters is: #{max_signal}"
end
