def read_input(filename):
    with open(filename, 'r') as file:
        heightmap = [list(map(int, list(line.strip()))) for line in file]
    return heightmap

def is_low_point(heightmap, row, col):
    # Get the height of the current point
    current_height = heightmap[row][col]
    
    # Define the possible directions: up, down, left, right
    directions = [(-1, 0), (1, 0), (0, -1), (0, 1)]
    
    for dr, dc in directions:
        adj_row, adj_col = row + dr, col + dc
        if 0 <= adj_row < len(heightmap) and 0 <= adj_col < len(heightmap[0]):
            # If any adjacent point is lower or equal, it's not a low point
            if heightmap[adj_row][adj_col] <= current_height:
                return False
    
    return True

def calculate_risk_level(heightmap):
    total_risk_level = 0
    
    # Iterate over the entire heightmap
    for row in range(len(heightmap)):
        for col in range(len(heightmap[0])):
            if is_low_point(heightmap, row, col):
                # Risk level is 1 + height of the low point
                total_risk_level += 1 + heightmap[row][col]
    
    return total_risk_level

if __name__ == "__main__":
    # Read input from the file
    heightmap = read_input('input_level_9.txt')
    
    # Calculate the total risk level
    total_risk_level = calculate_risk_level(heightmap)
    
    print(f"The total risk level is: {total_risk_level}")
