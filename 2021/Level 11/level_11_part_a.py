def read_input(filename):
    with open(filename, 'r') as file:
        grid = [list(map(int, line.strip())) for line in file]
    return grid

def increase_energy(grid):
    for row in range(len(grid)):
        for col in range(len(grid[0])):
            grid[row][col] += 1

def flash_octopus(grid, row, col, flashed):
    if flashed[row][col]:
        return 0  # This octopus has already flashed
    
    if grid[row][col] <= 9:
        return 0  # No flash for this octopus
    
    flashed[row][col] = True  # Mark this octopus as flashed
    flashes = 1  # Count this flash

    # Define directions: up, down, left, right, and all 4 diagonals
    directions = [(-1, 0), (1, 0), (0, -1), (0, 1), (-1, -1), (-1, 1), (1, -1), (1, 1)]
    
    # Increase the energy of all adjacent octopuses
    for dr, dc in directions:
        adj_row, adj_col = row + dr, col + dc
        if 0 <= adj_row < len(grid) and 0 <= adj_col < len(grid[0]):
            grid[adj_row][adj_col] += 1
            # Recursively check if this causes another flash
            flashes += flash_octopus(grid, adj_row, adj_col, flashed)
    
    return flashes

def step(grid):
    # Increase energy of all octopuses by 1
    increase_energy(grid)
    
    # Track which octopuses have flashed
    flashed = [[False for _ in range(len(grid[0]))] for _ in range(len(grid))]
    
    total_flashes = 0
    
    # Check for flashes
    for row in range(len(grid)):
        for col in range(len(grid[0])):
            if grid[row][col] > 9:
                total_flashes += flash_octopus(grid, row, col, flashed)
    
    # Reset flashed octopuses to 0
    for row in range(len(grid)):
        for col in range(len(grid[0])):
            if flashed[row][col]:
                grid[row][col] = 0
    
    return total_flashes

def simulate_octopus_flashes(grid, steps):
    total_flashes = 0
    for _ in range(steps):
        total_flashes += step(grid)
    return total_flashes

if __name__ == "__main__":
    # Read the input grid from file
    grid = read_input('input_level_11.txt')
    
    # Simulate 100 steps and count the total number of flashes
    total_flashes = simulate_octopus_flashes(grid, 100)
    
    print(f"Total flashes after 100 steps: {total_flashes}")
