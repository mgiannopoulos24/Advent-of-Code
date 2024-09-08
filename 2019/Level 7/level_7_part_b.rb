require 'set'

# Parsing program from file
def parse_program(filename)
  File.read(filename).strip.split(',').map(&:to_i)
end

# Amplifier class to maintain state
class Amplifier
  attr_accessor :program, :pointer, :inputs, :output, :halted

  def initialize(program, phase)
    @program = program.dup
    @pointer = 0
    @inputs = [phase]
    @output = nil
    @halted = false
  end

  def run_intcode
    while @pointer < @program.size
      opcode = @program[@pointer] % 100
      modes = [@program[@pointer] / 100 % 10, @program[@pointer] / 1000 % 10, @program[@pointer] / 10000 % 10]

      def get_param(program, pointer, index, mode)
        mode == 0 ? program[program[pointer + index]] : program[pointer + index]
      end

      case opcode
      when 1
        @program[@program[@pointer + 3]] = get_param(@program, @pointer, 1, modes[0]) + get_param(@program, @pointer, 2, modes[1])
        @pointer += 4
      when 2
        @program[@program[@pointer + 3]] = get_param(@program, @pointer, 1, modes[0]) * get_param(@program, @pointer, 2, modes[1])
        @pointer += 4
      when 3
        return if @inputs.empty? # Wait for more input
        @program[@program[@pointer + 1]] = @inputs.shift
        @pointer += 2
      when 4
        @output = get_param(@program, @pointer, 1, modes[0])
        @pointer += 2
        return @output # Return the output to allow feedback loop
      when 5
        @pointer = get_param(@program, @pointer, 2, modes[1]) if get_param(@program, @pointer, 1, modes[0]) != 0
      when 6
        @pointer = get_param(@program, @pointer, 2, modes[1]) if get_param(@program, @pointer, 1, modes[0]) == 0
      when 7
        @program[@program[@pointer + 3]] = get_param(@program, @pointer, 1, modes[0]) < get_param(@program, @pointer, 2, modes[1]) ? 1 : 0
        @pointer += 4
      when 8
        @program[@program[@pointer + 3]] = get_param(@program, @pointer, 1, modes[0]) == get_param(@program, @pointer, 2, modes[1]) ? 1 : 0
        @pointer += 4
      when 99
        @halted = true
        return @output
      else
        raise "Unknown opcode #{opcode}"
      end
    end
    @output
  end
end

# Run amplifiers in a feedback loop
def run_amplifiers_with_feedback(program, phase_settings)
  amplifiers = phase_settings.map { |phase| Amplifier.new(program, phase) }
  signal = 0
  amp_index = 0

  until amplifiers.all?(&:halted)
    amplifier = amplifiers[amp_index]
    amplifier.inputs << signal
    output = amplifier.run_intcode
    signal = output unless output.nil?
    amp_index = (amp_index + 1) % amplifiers.size
  end

  signal
end

# Find the maximum signal for phase settings 5..9 with feedback loop
def find_max_signal_with_feedback(program)
  max_signal = 0
  permutations = (5..9).to_a.permutation.to_a

  permutations.each do |phase_settings|
    signal = run_amplifiers_with_feedback(program, phase_settings)
    max_signal = [max_signal, signal].max
  end

  max_signal
end

# Main execution
if __FILE__ == $PROGRAM_NAME
  filename = 'input_level_7.txt'
  program = parse_program(filename)
  max_signal = find_max_signal_with_feedback(program)
  puts "The maximum signal that can be sent to the thrusters is: #{max_signal}"
end
