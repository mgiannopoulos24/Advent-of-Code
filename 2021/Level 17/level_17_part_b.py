def simulate_probe(vx, vy, target_x_range, target_y_range):
    x, y = 0, 0
    
    while x <= target_x_range[1] and y >= target_y_range[0]:
        # Update positions
        x += vx
        y += vy

        # Check if the probe is in the target area
        if target_x_range[0] <= x <= target_x_range[1] and target_y_range[0] <= y <= target_y_range[1]:
            return True

        # Update velocities
        if vx > 0:
            vx -= 1
        elif vx < 0:
            vx += 1
        vy -= 1

    return False

def find_valid_velocities(target_x_range, target_y_range):
    valid_velocities = set()

    # Iterate over a reasonable range of initial velocities
    for vx in range(1, target_x_range[1] + 1):
        for vy in range(target_y_range[0], 1000):  # Use an upper limit for vy
            if simulate_probe(vx, vy, target_x_range, target_y_range):
                valid_velocities.add((vx, vy))

    return valid_velocities

if __name__ == "__main__":
    target_x_range = (169, 206)  # Change this to the actual target range
    target_y_range = (-108, -68)  # Change this to the actual target range
    
    # Find all valid initial velocities
    valid_velocities = find_valid_velocities(target_x_range, target_y_range)
    
    print(f"Number of distinct valid velocities: {len(valid_velocities)}")
