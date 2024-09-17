def find_rating(report, rating_type):
    # Initialize the filtering process
    remaining = report[:]
    bit_length = len(report[0])

    # Iterate over each bit position
    for bit_position in range(bit_length):
        if len(remaining) == 1:
            break
        
        # Count the number of 1s and 0s in the current bit position
        bit_count = sum(1 for number in remaining if number[bit_position] == '1')
        zero_count = len(remaining) - bit_count
        
        # Determine the filtering criteria based on the rating type
        if rating_type == 'oxygen':
            # Most common: keep '1' if bit_count >= zero_count, otherwise keep '0'
            most_common_bit = '1' if bit_count >= zero_count else '0'
            remaining = [num for num in remaining if num[bit_position] == most_common_bit]
        elif rating_type == 'co2':
            # Least common: keep '0' if bit_count >= zero_count, otherwise keep '1'
            least_common_bit = '0' if bit_count >= zero_count else '1'
            remaining = [num for num in remaining if num[bit_position] == least_common_bit]

    # Return the final remaining number as an integer
    return int(remaining[0], 2)

def calculate_life_support_rating(filename: str) -> int:
    # Read the binary numbers from the file
    with open(filename, 'r') as file:
        report = [line.strip() for line in file]

    # Find the oxygen generator rating
    oxygen_generator_rating = find_rating(report, 'oxygen')

    # Find the CO2 scrubber rating
    co2_scrubber_rating = find_rating(report, 'co2')

    # Calculate and return the life support rating (product of the two)
    return oxygen_generator_rating * co2_scrubber_rating

if __name__ == "__main__":
    input_file = 'input_level_3.txt'  # Replace with your input file
    result = calculate_life_support_rating(input_file)
    print(f"Life support rating: {result}")
