local function parse_input(file_path)
    local positions = {}
    for line in io.lines(file_path) do
        local x, y = line:match("([^,]+),([^,]+)")
        table.insert(positions, { tonumber(x), tonumber(y) })
    end
    return positions
end

local function simulate_falling_bytes_until_blocked(corrupt_positions, grid_size)
    local grid = {}
    for i = 1, grid_size do
        grid[i] = {}
        for j = 1, grid_size do
            grid[i][j] = '.'
        end
    end

    local function is_path_blocked()
        local start = {0, 0}
        local end_pos = {grid_size - 1, grid_size - 1}

        if grid[1][1] == '#' or grid[end_pos[2] + 1][end_pos[1] + 1] == '#' then
            return true  -- Blocked if start or end is corrupted
        end

        local directions = {{0, 1}, {1, 0}, {0, -1}, {-1, 0}}  -- Down, Right, Up, Left
        local queue = {{start, 0}}  -- (current position, steps)
        local visited = {}
        visited[start[1] * grid_size + start[2]] = true

        while #queue > 0 do
            local current = table.remove(queue, 1)
            local x, y = current[1][1], current[1][2]

            if x == end_pos[1] and y == end_pos[2] then
                return false  -- Path is still open
            end

            for _, direction in ipairs(directions) do
                local nx, ny = x + direction[1], y + direction[2]
                if nx >= 0 and nx < grid_size and ny >= 0 and ny < grid_size then
                    if not visited[nx * grid_size + ny] and grid[ny + 1][nx + 1] == '.' then
                        visited[nx * grid_size + ny] = true
                        table.insert(queue, {{nx, ny}, 0})
                    end
                end
            end
        end

        return true  -- No path found
    end

    for i, pos in ipairs(corrupt_positions) do
        local x, y = pos[1], pos[2]
        grid[y + 1][x + 1] = '#'  -- Mark the position as corrupted

        if is_path_blocked() then
            return x, y  -- Return the first byte that blocks the path
        end
    end

    return -1, -1  -- Path never gets completely blocked
end

local function main()
    local input_file = "input_level_18.txt"  
    local corrupt_positions = parse_input(input_file)

    local grid_size = 71  -- Grid is 71x71 (0 to 70 inclusive)

    -- Find the first byte that blocks the path
    local blocking_byte_x, blocking_byte_y = simulate_falling_bytes_until_blocked(corrupt_positions, grid_size)

    if blocking_byte_x == -1 and blocking_byte_y == -1 then
        print("Path never gets completely blocked")
    else
        print(string.format("The coordinates are: %d,%d", blocking_byte_x, blocking_byte_y))
    end
end

main()
