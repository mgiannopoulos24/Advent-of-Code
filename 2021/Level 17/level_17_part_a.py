def simulate_probe(vx, vy, target_x_range, target_y_range):
    x, y = 0, 0
    max_y = 0
    
    while x <= target_x_range[1] and y >= target_y_range[0]:
        # Update positions
        x += vx
        y += vy
        max_y = max(max_y, y)

        # Check if we're in the target area
        if target_x_range[0] <= x <= target_x_range[1] and target_y_range[0] <= y <= target_y_range[1]:
            return True, max_y
        
        # Update velocities
        if vx > 0:
            vx -= 1
        elif vx < 0:
            vx += 1
        
        vy -= 1

    return False, max_y

def find_highest_y(target_x_range, target_y_range):
    highest_y = 0
    best_velocity = (0, 0)

    for vx in range(1, target_x_range[1] + 1):
        for vy in range(target_y_range[0], 1000):
            hit, max_y = simulate_probe(vx, vy, target_x_range, target_y_range)
            if hit and max_y > highest_y:
                highest_y = max_y
                best_velocity = (vx, vy)

    return highest_y, best_velocity

if __name__ == "__main__":
    target_x_range = (169, 206)  # Change this to the actual target range
    target_y_range = (-108, -68)  # Change this to the actual target range
    
    # Find the highest Y value
    highest_y, best_velocity = find_highest_y(target_x_range, target_y_range)
    
    print(f"The highest Y position reached is: {highest_y}")
    print(f"The best velocity is: {best_velocity}")
