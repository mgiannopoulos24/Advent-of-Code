def calculate_position(filename: str) -> int:
    # Initialize the horizontal position and depth
    horizontal_position = 0
    depth = 0

    # Read the commands from the input file
    with open(filename, 'r') as file:
        for line in file:
            command, value = line.strip().split()  # Split the command into direction and value
            value = int(value)  # Convert the value to an integer

            # Process the command
            if command == 'forward':
                horizontal_position += value
            elif command == 'down':
                depth += value
            elif command == 'up':
                depth -= value

    # Calculate the final result
    return horizontal_position * depth

if __name__ == "__main__":
    input_file = 'input_level_2.txt'  # Replace with your input file
    result = calculate_position(input_file)
    print(f"Final horizontal position * depth: {result}")
