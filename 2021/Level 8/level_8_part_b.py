def read_input(filename):
    with open(filename, 'r') as file:
        lines = file.readlines()
    return [line.strip().split(' | ') for line in lines]

def decode_signals(signals):
    # Sort signals by length for easier pattern identification
    signals = [''.join(sorted(signal)) for signal in signals]
    
    # Group signals by their length
    signals_by_length = {length: [] for length in range(2, 8)}
    
    for signal in signals:
        signals_by_length[len(signal)].append(signal)

    # Step 1: Identify digits 1, 4, 7, 8 based on their unique lengths
    one = signals_by_length[2][0]
    four = signals_by_length[4][0]
    seven = signals_by_length[3][0]
    eight = signals_by_length[7][0]

    # Step 2: Deduce other digits
    # 6-segment digits: 0, 6, 9
    six_segment_digits = signals_by_length[6]
    nine = next(s for s in six_segment_digits if all(c in s for c in four))  # 9 contains all segments of 4
    zero = next(s for s in six_segment_digits if all(c in s for c in one) and s != nine)  # 0 contains all segments of 1 but is not 9
    six = next(s for s in six_segment_digits if s != nine and s != zero)  # The remaining 6-segment digit is 6

    # 5-segment digits: 2, 3, 5
    five_segment_digits = signals_by_length[5]
    three = next(s for s in five_segment_digits if all(c in s for c in one))  # 3 contains all segments of 1
    five = next(s for s in five_segment_digits if all(c in six for c in s))  # 5 is fully contained within 6
    two = next(s for s in five_segment_digits if s != three and s != five)  # The remaining 5-segment digit is 2

    # Create a mapping of patterns to digits
    pattern_to_digit = {
        ''.join(sorted(zero)): 0,
        ''.join(sorted(one)): 1,
        ''.join(sorted(two)): 2,
        ''.join(sorted(three)): 3,
        ''.join(sorted(four)): 4,
        ''.join(sorted(five)): 5,
        ''.join(sorted(six)): 6,
        ''.join(sorted(seven)): 7,
        ''.join(sorted(eight)): 8,
        ''.join(sorted(nine)): 9,
    }

    return pattern_to_digit


def decode_output(pattern_to_digit, output_values):
    # Decode each output value using the pattern_to_digit mapping
    decoded_value = int(''.join(str(pattern_to_digit[''.join(sorted(output))]) for output in output_values))
    return decoded_value

def sum_decoded_outputs(data):
    total_sum = 0

    for entry in data:
        signal_patterns, output_values = entry
        signal_patterns = signal_patterns.split()
        output_values = output_values.split()

        # Step 1: Decode the signals to find the pattern-to-digit mapping
        pattern_to_digit = decode_signals(signal_patterns)

        # Step 2: Decode the output values using the pattern-to-digit mapping
        decoded_value = decode_output(pattern_to_digit, output_values)

        # Step 3: Add the decoded value to the total sum
        total_sum += decoded_value

    return total_sum

if __name__ == "__main__":
    # Read input from file
    data = read_input('input_level_8.txt')

    # Decode all the outputs and calculate the sum
    result = sum_decoded_outputs(data)

    print(f"Total sum of decoded output values: {result}")
