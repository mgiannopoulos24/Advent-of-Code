def count_sliding_window_increases(filename: str) -> int:
    # Read the depths from the file and convert them to integers
    with open(filename, 'r') as file:
        depths = [int(line.strip()) for line in file]

    # Initialize a counter for the number of increases
    increase_count = 0

    # Loop through the list and compare sums of sliding windows of 3 measurements
    for i in range(1, len(depths) - 2):
        # Sum of the current window (depths[i-1], depths[i], depths[i+1])
        previous_window_sum = depths[i - 1] + depths[i] + depths[i + 1]
        # Sum of the next window (depths[i], depths[i+1], depths[i+2])
        current_window_sum = depths[i] + depths[i + 1] + depths[i + 2]

        # Compare the current window sum with the previous window sum
        if current_window_sum > previous_window_sum:
            increase_count += 1

    return increase_count

if __name__ == "__main__":
    input_file = 'input_level_1.txt'  # Replace with the actual input file name
    result = count_sliding_window_increases(input_file)
    print(f"Number of sliding window sum increases: {result}")
