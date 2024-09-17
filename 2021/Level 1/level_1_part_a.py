def count_increases_in_depth(filename: str) -> int:
    # Read the depths from the file and convert them to integers
    with open(filename, 'r') as file:
        depths = [int(line.strip()) for line in file]

    # Initialize a counter for the number of increases
    increase_count = 0

    # Loop through the depths starting from the second element
    for i in range(1, len(depths)):
        # If the current depth is greater than the previous one, increment the counter
        if depths[i] > depths[i - 1]:
            increase_count += 1

    return increase_count

if __name__ == "__main__":
    input_file = 'input_level_1.txt'  # Replace with the actual input file name
    result = count_increases_in_depth(input_file)
    print(f"Number of depth increases: {result}")
