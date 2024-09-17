def read_input(filename):
    with open(filename, 'r') as file:
        lines = file.readlines()
    return lines

def count_unique_digit_appearances(lines):
    # Unique segment lengths for digits 1, 4, 7, 8
    unique_lengths = {2, 3, 4, 7}
    count = 0
    
    for line in lines:
        # Split the line by ' | ' to separate signal patterns and output values
        parts = line.strip().split(' | ')
        output_values = parts[1].split()
        
        # Count how many times the output values have unique segment lengths
        for value in output_values:
            if len(value) in unique_lengths:
                count += 1
    
    return count

if __name__ == "__main__":
    # Read input from the file
    lines = read_input('input_level_8.txt')
    
    # Count the appearances of digits 1, 4, 7, or 8 in the output values
    result = count_unique_digit_appearances(lines)
    
    print(f"Total count of digits 1, 4, 7, or 8 in the output values: {result}")
