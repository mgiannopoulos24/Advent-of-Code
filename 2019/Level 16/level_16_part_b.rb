def read_signal(filename)
    # Reads the signal from the file and converts it into an array of integers
    File.read(filename).strip.chars.map(&:to_i)
  end
  
  def repeat_signal(signal, times)
    # Repeat the signal `times` times
    signal * times
  end
  
  def fft_optimized(signal, phases, offset)
    # Start from the message offset, which is in the second half of the signal
    # Only apply FFT on the second half as the pattern simplifies to 1s
    signal = signal[offset..-1]
    
    phases.times do
      # Using reverse cumulative sum to update the signal
      partial_sum = 0
      (signal.length - 1).downto(0) do |i|
        partial_sum = (partial_sum + signal[i]) % 10
        signal[i] = partial_sum
      end
    end
    
    signal
  end
  
  def first_eight_digits_after_phases(filename, phases)
    # Read the original signal
    signal = read_signal(filename)
    
    # The message offset is represented by the first 7 digits of the input signal
    offset = signal[0, 7].join.to_i
    
    # Repeat the signal 10000 times
    large_signal = repeat_signal(signal, 10000)
    
    # Apply the optimized FFT to find the eight digits starting from the offset
    result_signal = fft_optimized(large_signal, phases, offset)
    
    # Return the first 8 digits from the offset position
    result_signal[0, 8].join
  end
  
  # Usage
  filename = 'input_level_16.txt' 
  phases = 100
  puts "Eight-digit message: #{first_eight_digits_after_phases(filename, phases)}"
  