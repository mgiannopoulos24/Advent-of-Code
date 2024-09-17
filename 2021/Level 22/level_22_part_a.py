def parse_input(file_name):
    reboot_steps = []
    with open(file_name, 'r') as file:
        for line in file:
            action, ranges = line.strip().split(" ")
            x_range, y_range, z_range = ranges.split(',')
            x_min, x_max = map(int, x_range[2:].split(".."))
            y_min, y_max = map(int, y_range[2:].split(".."))
            z_min, z_max = map(int, z_range[2:].split(".."))
            reboot_steps.append((action, (x_min, x_max, y_min, y_max, z_min, z_max)))
    return reboot_steps

# Function to clamp the ranges to the initialization area (-50..50)
def clamp_range(value_min, value_max, clamp_min, clamp_max):
    value_min = max(value_min, clamp_min)
    value_max = min(value_max, clamp_max)
    if value_min > value_max:
        return None  # Range is completely outside the clamp region
    return (value_min, value_max)

# Function to process the reboot steps and count the number of "on" cubes
def process_reboot_steps(reboot_steps):
    cubes_on = set()

    # Iterate over each step
    for action, (x_min, x_max, y_min, y_max, z_min, z_max) in reboot_steps:
        # Clamp the ranges to -50..50
        x_range = clamp_range(x_min, x_max, -50, 50)
        y_range = clamp_range(y_min, y_max, -50, 50)
        z_range = clamp_range(z_min, z_max, -50, 50)

        # Skip if any range is completely outside the initialization region
        if x_range is None or y_range is None or z_range is None:
            continue

        # Process the clamped ranges
        x_min, x_max = x_range
        y_min, y_max = y_range
        z_min, z_max = z_range

        if action == "on":
            for x in range(x_min, x_max + 1):
                for y in range(y_min, y_max + 1):
                    for z in range(z_min, z_max + 1):
                        cubes_on.add((x, y, z))
        elif action == "off":
            for x in range(x_min, x_max + 1):
                for y in range(y_min, y_max + 1):
                    for z in range(z_min, z_max + 1):
                        cubes_on.discard((x, y, z))

    # Return the number of cubes that are "on"
    return len(cubes_on)

# Main function to tie everything together
def main(file_name):
    reboot_steps = parse_input(file_name)
    result = process_reboot_steps(reboot_steps)
    print(f"Number of cubes that are on: {result}")

# Example usage:
if __name__ == "__main__":
    main("input_level_22.txt")
