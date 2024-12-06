local function parse_input(file)
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

local function simulate_guard(map_data)
    local rows = #map_data
    local cols = #map_data[1]
    local directions = { {-1, 0}, {0, 1}, {1, 0}, {0, -1} } -- Up, Right, Down, Left
    local direction = 1 -- Initially facing up (index in Lua starts at 1)
    local visited = {}

    -- Function to mark a position as visited
    local function visit(r, c)
        visited[r .. "," .. c] = true
    end

    -- Find the guard's starting position
    local guard_pos
    for r = 1, rows do
        for c = 1, cols do
            if map_data[r][c] == "^" or map_data[r][c] == ">" or map_data[r][c] == "v" or map_data[r][c] == "<" then
                guard_pos = {r, c}
                visit(r, c)
                break
            end
        end
        if guard_pos then break end
    end

    while true do
        local r, c = guard_pos[1], guard_pos[2]
        local dr, dc = directions[direction][1], directions[direction][2]
        local nr, nc = r + dr, c + dc

        -- Check if the guard is out of bounds
        if nr < 1 or nr > rows or nc < 1 or nc > cols then
            break
        end

        -- Check if there's an obstacle
        if map_data[nr][nc] == "#" then
            direction = (direction % 4) + 1 -- Turn right
        else
            guard_pos = {nr, nc}
            visit(nr, nc)
        end
    end

    -- Count distinct visited positions
    local count = 0
    for _ in pairs(visited) do
        count = count + 1
    end
    return count
end

local function main()
    local map_data = parse_input("input_level_6.txt")
    local result = simulate_guard(map_data)
    print("Distinct positions visited: " .. result)
end

main()
