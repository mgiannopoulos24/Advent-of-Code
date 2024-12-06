local function parse_input(file)
    -- Parses the input file and returns the map as a 2D array
    local map_data = {}
    for line in io.lines(file) do
        local row = {}
        for char in line:gmatch(".") do
            table.insert(row, char)
        end
        table.insert(map_data, row)
    end
    return map_data
end

local function guard_stuck_in_loop(map_data, guard_pos)
    -- Simulates the guard's movement and checks if it gets stuck in a loop
    local rows = #map_data
    local cols = #map_data[1]
    local directions = { {-1, 0}, {0, 1}, {1, 0}, {0, -1} } -- Up, Right, Down, Left
    local direction = 1 -- Initially facing up (Lua indices start at 1)
    local visited = {} -- Track visited positions with directions

    local r, c = guard_pos[1], guard_pos[2]
    visited[r .. "," .. c .. "," .. direction] = true -- Add initial position and direction

    while true do
        local dr, dc = directions[direction][1], directions[direction][2]
        local nr, nc = r + dr, c + dc

        -- Check if the guard is out of bounds
        if nr < 1 or nr > rows or nc < 1 or nc > cols then
            return false -- Not a loop
        end

        -- Check if there's an obstacle
        if map_data[nr][nc] == "#" then
            direction = (direction % 4) + 1 -- Turn right
        else
            r, c = nr, nc -- Move forward
        end

        -- Check if the position and direction have been visited
        local key = r .. "," .. c .. "," .. direction
        if visited[key] then
            return true -- Loop detected
        end

        visited[key] = true
    end
end

local function count_valid_obstructions(map_data)
    -- Counts valid positions where an obstruction can trap the guard in a loop
    local rows = #map_data
    local cols = #map_data[1]
    local guard_pos

    -- Find the guard's starting position
    for r = 1, rows do
        for c = 1, cols do
            local cell = map_data[r][c]
            if cell == "^" or cell == ">" or cell == "v" or cell == "<" then
                guard_pos = {r, c}
                break
            end
        end
        if guard_pos then break end
    end

    if not guard_pos then
        error("Guard not found in the map.")
    end

    local valid_positions = 0

    -- Try placing an obstruction at every open position
    for r = 1, rows do
        for c = 1, cols do
            if map_data[r][c] == "." and not (r == guard_pos[1] and c == guard_pos[2]) then
                -- Temporarily place obstruction
                map_data[r][c] = "#"

                -- Check if the guard gets stuck in a loop
                if guard_stuck_in_loop(map_data, guard_pos) then
                    valid_positions = valid_positions + 1
                end

                -- Remove the temporary obstruction
                map_data[r][c] = "."
            end
        end
    end

    return valid_positions
end

local function main()
    local map_data = parse_input("input_level_6.txt")
    local result = count_valid_obstructions(map_data)
    print("Number of valid obstruction positions: " .. result)
end

main()
