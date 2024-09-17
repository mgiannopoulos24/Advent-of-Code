def read_input(filename):
    # Read the input from the file and return it as a 2D list
    with open(filename, 'r') as file:
        return [list(line.strip()) for line in file.readlines()]

def move_east(grid):
    rows, cols = len(grid), len(grid[0])
    moved = False
    new_grid = [row[:] for row in grid]  # Copy the grid
    
    # Move all east-facing cucumbers
    for r in range(rows):
        for c in range(cols):
            if grid[r][c] == '>' and grid[r][(c + 1) % cols] == '.':
                new_grid[r][c] = '.'
                new_grid[r][(c + 1) % cols] = '>'
                moved = True
    return new_grid, moved

def move_south(grid):
    rows, cols = len(grid), len(grid[0])
    moved = False
    new_grid = [row[:] for row in grid]  # Copy the grid
    
    # Move all south-facing cucumbers
    for r in range(rows):
        for c in range(cols):
            if grid[r][c] == 'v' and grid[(r + 1) % rows][c] == '.':
                new_grid[r][c] = '.'
                new_grid[(r + 1) % rows][c] = 'v'
                moved = True
    return new_grid, moved

def simulate_cucumbers(grid):
    step = 0
    while True:
        step += 1
        # Move east-facing cucumbers
        grid, east_moved = move_east(grid)
        # Move south-facing cucumbers
        grid, south_moved = move_south(grid)
        # If no cucumbers moved in either direction, stop
        if not east_moved and not south_moved:
            return step

if __name__ == "__main__":
    # Read the input grid from the file
    grid = read_input('input_level_25.txt')
    
    # Simulate the sea cucumber movements and find the step when they stop
    result = simulate_cucumbers(grid)
    
    # Output the result
    print(f"The first step on which no sea cucumbers move is: {result}")
