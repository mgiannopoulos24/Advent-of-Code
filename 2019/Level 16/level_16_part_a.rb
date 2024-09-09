def read_signal(filename)
    # Reads the signal from the file and converts it into an array of integers
    File.read(filename).strip.chars.map(&:to_i)
  end
  
  def base_pattern
    # Base pattern that repeats: [0, 1, 0, -1]
    [0, 1, 0, -1]
  end
  
  def apply_pattern(signal, phases)
    # Iterate for the number of phases
    phases.times do
      new_signal = []
      
      signal.length.times do |i|
        pattern = generate_pattern(i + 1, signal.length)
        # Calculate the sum of signal[i] * pattern[j]
        new_value = signal.each_with_index.reduce(0) do |sum, (val, idx)|
          sum + val * pattern[idx]
        end
        # Append the ones digit of the sum to the new signal
        new_signal << new_value.abs % 10
      end
  
      # Set the signal for the next phase
      signal = new_signal
    end
  
    signal
  end
  
  def generate_pattern(position, length)
    # Generate the repeated pattern for the current position
    pattern = base_pattern.flat_map { |p| [p] * position }
    pattern_cycle = pattern.cycle
    # Skip the very first element of the pattern
    pattern_cycle.next
    # Generate only the required length pattern
    Array.new(length) { pattern_cycle.next }
  end
  
  def fft(signal, phases)
    # Perform FFT algorithm on the signal for the specified number of phases
    apply_pattern(signal, phases)
  end
  
  def first_eight_digits_after_phases(filename, phases)
    # Read signal and apply FFT
    signal = read_signal(filename)
    final_signal = fft(signal, phases)
    # Return the first 8 digits
    final_signal[0, 8].join
  end
  
  # Usage
  filename = 'input_level_16.txt' 
  phases = 100
  puts "First 8 digits after #{phases} phases: #{first_eight_digits_after_phases(filename, phases)}"
  