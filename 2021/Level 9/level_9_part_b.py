def read_input(filename):
    with open(filename, 'r') as file:
        heightmap = [list(map(int, list(line.strip()))) for line in file]
    return heightmap

def is_low_point(heightmap, row, col):
    current_height = heightmap[row][col]
    directions = [(-1, 0), (1, 0), (0, -1), (0, 1)]
    
    for dr, dc in directions:
        adj_row, adj_col = row + dr, col + dc
        if 0 <= adj_row < len(heightmap) and 0 <= adj_col < len(heightmap[0]):
            if heightmap[adj_row][adj_col] <= current_height:
                return False
    return True

def find_low_points(heightmap):
    low_points = []
    for row in range(len(heightmap)):
        for col in range(len(heightmap[0])):
            if is_low_point(heightmap, row, col):
                low_points.append((row, col))
    return low_points

def dfs(heightmap, row, col, visited):
    if (row, col) in visited:
        return 0
    if heightmap[row][col] == 9:
        return 0
    
    visited.add((row, col))
    basin_size = 1
    
    # Explore neighbors (up, down, left, right)
    directions = [(-1, 0), (1, 0), (0, -1), (0, 1)]
    for dr, dc in directions:
        adj_row, adj_col = row + dr, col + dc
        if 0 <= adj_row < len(heightmap) and 0 <= adj_col < len(heightmap[0]):
            basin_size += dfs(heightmap, adj_row, adj_col, visited)
    
    return basin_size

def calculate_basin_sizes(heightmap, low_points):
    visited = set()
    basin_sizes = []
    
    for row, col in low_points:
        basin_size = dfs(heightmap, row, col, visited)
        basin_sizes.append(basin_size)
    
    return basin_sizes

if __name__ == "__main__":
    # Read input from file
    heightmap = read_input('input_level_9.txt')
    
    # Find all low points
    low_points = find_low_points(heightmap)
    
    # Calculate basin sizes
    basin_sizes = calculate_basin_sizes(heightmap, low_points)
    
    # Find the three largest basins
    largest_basins = sorted(basin_sizes, reverse=True)[:3]
    
    # Multiply the sizes of the three largest basins
    result = largest_basins[0] * largest_basins[1] * largest_basins[2]
    
    print(f"The product of the sizes of the three largest basins is: {result}")
