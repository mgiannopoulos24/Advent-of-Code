local function read_input(file_path)
    local grid = {}
    for line in io.lines(file_path) do
        local row = {}
        for num in line:gmatch("%d") do
            table.insert(row, tonumber(num))
        end
        table.insert(grid, row)
    end
    return grid
end

local function is_valid(x, y, grid)
    return x >= 1 and x <= #grid and y >= 1 and y <= #grid[1]
end

local function find_score(grid, start_x, start_y)
    local directions = {
        {0, 1},  -- Right
        {1, 0},  -- Down
        {0, -1}, -- Left
        {-1, 0}  -- Up
    }
    local queue = {{start_x, start_y}}
    local visited = {}
    local score = 0

    -- Initialize visited set
    visited[start_x] = visited[start_x] or {}
    visited[start_x][start_y] = true

    while #queue > 0 do
        local current = table.remove(queue, 1)
        local x, y = current[1], current[2]

        if grid[x][y] == 9 then
            score = score + 1
        end

        for _, dir in ipairs(directions) do
            local nx, ny = x + dir[1], y + dir[2]
            if is_valid(nx, ny, grid) and not (visited[nx] and visited[nx][ny]) and grid[nx][ny] == grid[x][y] + 1 then
                visited[nx] = visited[nx] or {}
                visited[nx][ny] = true
                table.insert(queue, {nx, ny})
            end
        end
    end

    return score
end

local function compute_trailhead_scores(grid)
    local total_score = 0
    for i = 1, #grid do
        for j = 1, #grid[1] do
            if grid[i][j] == 0 then -- Found a trailhead
                total_score = total_score + find_score(grid, i, j)
            end
        end
    end
    return total_score
end

local input_file = "input_level_10.txt"
local topographic_map = read_input(input_file)
local result = compute_trailhead_scores(topographic_map)
print("Sum of the scores of all trailheads:", result)
