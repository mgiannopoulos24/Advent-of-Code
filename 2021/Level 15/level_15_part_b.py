import heapq

def read_input(filename):
    with open(filename, 'r') as file:
        grid = [list(map(int, line.strip())) for line in file.readlines()]
    return grid

def increase_risk(value, increment):
    return (value + increment - 1) % 9 + 1

def expand_grid(grid, multiplier=5):
    original_rows = len(grid)
    original_cols = len(grid[0])
    expanded_grid = [[0 for _ in range(original_cols * multiplier)] for _ in range(original_rows * multiplier)]

    for i in range(multiplier):
        for j in range(multiplier):
            for row in range(original_rows):
                for col in range(original_cols):
                    new_value = increase_risk(grid[row][col], i + j)
                    expanded_grid[row + i * original_rows][col + j * original_cols] = new_value
    
    return expanded_grid

def dijkstra(grid):
    rows, cols = len(grid), len(grid[0])
    
    # Priority queue to store (risk, x, y) tuples
    pq = [(0, 0, 0)]  # Start at the top-left corner with risk 0
    min_risks = {(0, 0): 0}  # Dictionary to store the minimum risk to each cell
    
    # Dijkstra's algorithm using a priority queue
    while pq:
        current_risk, x, y = heapq.heappop(pq)
        
        # If we reach the bottom-right corner, return the risk
        if (x, y) == (rows - 1, cols - 1):
            return current_risk
        
        # Explore neighbors (up, down, left, right)
        for dx, dy in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
            nx, ny = x + dx, y + dy
            if 0 <= nx < rows and 0 <= ny < cols:
                new_risk = current_risk + grid[nx][ny]
                
                # If this path to (nx, ny) is less risky, update and push to the queue
                if (nx, ny) not in min_risks or new_risk < min_risks[(nx, ny)]:
                    min_risks[(nx, ny)] = new_risk
                    heapq.heappush(pq, (new_risk, nx, ny))
    
    # If we exhaust the queue without reaching the end, something went wrong
    return -1

if __name__ == "__main__":
    # Read the input grid
    grid = read_input('input_level_15.txt')
    
    # Expand the grid to 5x5 times the original size
    expanded_grid = expand_grid(grid, multiplier=5)
    
    # Find the lowest total risk using Dijkstra's algorithm on the expanded grid
    lowest_risk = dijkstra(expanded_grid)
    
    print(f"The lowest total risk of any path is: {lowest_risk}")
