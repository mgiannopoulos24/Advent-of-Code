import statistics

def read_input(filename):
    with open(filename, 'r') as file:
        positions = list(map(int, file.readline().strip().split(',')))
    return positions

def calculate_fuel_cost(positions, target_position):
    return sum(abs(pos - target_position) for pos in positions)

def find_optimal_position(positions):
    # The optimal position to align to is the median of the positions
    median_position = int(statistics.median(positions))
    return median_position

if __name__ == "__main__":
    # Read input from the file
    positions = read_input('input_level_7.txt')
    
    # Find the optimal position (median)
    optimal_position = find_optimal_position(positions)
    
    # Calculate the total fuel cost to move all crabs to that position
    total_fuel = calculate_fuel_cost(positions, optimal_position)
    
    print(f"Total fuel cost to align crabs: {total_fuel}")
