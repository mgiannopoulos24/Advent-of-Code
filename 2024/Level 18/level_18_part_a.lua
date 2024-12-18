local function parse_input(file_path)
    local positions = {}
    for line in io.lines(file_path) do
        local x, y = line:match("([^,]+),([^,]+)")
        table.insert(positions, { tonumber(x), tonumber(y) })
    end
    return positions
end

local function simulate_falling_bytes(corrupt_positions, grid_size, num_bytes)
    local grid = {}
    for i = 1, grid_size do
        grid[i] = {}
        for j = 1, grid_size do
            grid[i][j] = '.'
        end
    end

    for i = 1, math.min(num_bytes, #corrupt_positions) do
        local x, y = corrupt_positions[i][1], corrupt_positions[i][2]
        grid[y + 1][x + 1] = '#'  -- Mark the position as corrupted (Lua is 1-indexed)
    end
    return grid
end

local function find_shortest_path(grid)
    local grid_size = #grid
    local start = {0, 0}
    local end_pos = {grid_size - 1, grid_size - 1}

    if grid[1][1] == '#' or grid[end_pos[2] + 1][end_pos[1] + 1] == '#' then
        return -1  -- No path if start or end is corrupted
    end

    local directions = {{0, 1}, {1, 0}, {0, -1}, {-1, 0}}  -- Down, Right, Up, Left
    local queue = {{start, 0}}  -- (current position, steps)
    local visited = {}
    visited[start[1] * grid_size + start[2]] = true

    while #queue > 0 do
        local current = table.remove(queue, 1)
        local x, y = current[1][1], current[1][2]
        local steps = current[2]

        if x == end_pos[1] and y == end_pos[2] then
            return steps
        end

        for _, direction in ipairs(directions) do
            local nx, ny = x + direction[1], y + direction[2]
            if nx >= 0 and nx < grid_size and ny >= 0 and ny < grid_size then
                if not visited[nx * grid_size + ny] and grid[ny + 1][nx + 1] == '.' then
                    visited[nx * grid_size + ny] = true
                    table.insert(queue, {{nx, ny}, steps + 1})
                end
            end
        end
    end

    return -1  -- No path found
end

local function main()
    local input_file = "input_level_18.txt"  
    local corrupt_positions = parse_input(input_file)

    local grid_size = 71  -- Grid is 71x71 (0 to 70 inclusive)
    local num_bytes = 1024  -- First kilobyte

    -- Simulate the falling bytes
    local grid = simulate_falling_bytes(corrupt_positions, grid_size, num_bytes)

    -- Find the shortest path
    local shortest_path_length = find_shortest_path(grid)

    print("Minimum number of steps needed to reach the exit:", shortest_path_length)
end

main()
