import math

def read_input(filename):
    with open(filename, 'r') as file:
        positions = list(map(int, file.readline().strip().split(',')))
    return positions

def calculate_fuel_cost(positions, target_position):
    # This function now calculates the non-linear fuel cost (1 + 2 + 3 + ... + d) = d * (d + 1) / 2
    return sum((abs(pos - target_position) * (abs(pos - target_position) + 1)) // 2 for pos in positions)

def find_optimal_position(positions):
    # The best possible position to minimize the fuel is around the mean
    mean_position = sum(positions) / len(positions)
    
    # Check both the floor and ceiling of the mean
    floor_position = math.floor(mean_position)
    ceil_position = math.ceil(mean_position)
    
    # Calculate the fuel cost for both floor and ceiling positions
    fuel_cost_floor = calculate_fuel_cost(positions, floor_position)
    fuel_cost_ceil = calculate_fuel_cost(positions, ceil_position)
    
    # Return the minimum of the two
    if fuel_cost_floor < fuel_cost_ceil:
        return fuel_cost_floor
    else:
        return fuel_cost_ceil

if __name__ == "__main__":
    # Read input from the file
    positions = read_input('input_level_7.txt')
    
    # Find the optimal position and calculate the fuel cost
    total_fuel = find_optimal_position(positions)
    
    print(f"Total fuel cost to align crabs: {total_fuel}")
