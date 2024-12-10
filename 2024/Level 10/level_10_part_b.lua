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

local function path_to_string(path)
    local parts = {}
    for _, coords in ipairs(path) do
        table.insert(parts, coords[1] .. "," .. coords[2]) -- Convert {x, y} to "x,y"
    end
    return table.concat(parts, "|") -- Join coordinates with "|"
end

local function find_trailhead_rating(grid, start_x, start_y)
    local directions = {
        {0, 1},  -- Right
        {1, 0},  -- Down
        {0, -1}, -- Left
        {-1, 0}  -- Up
    }
    local stack = {{start_x, start_y, {{start_x, start_y}}}} -- (x, y, path)
    local all_paths = {}

    while #stack > 0 do
        local current = table.remove(stack)
        local x, y, path = current[1], current[2], current[3]

        -- If the height is 9, record the path as a distinct trail
        if grid[x][y] == 9 then
            local path_key = path_to_string(path)
            all_paths[path_key] = true
        else
            for _, dir in ipairs(directions) do
                local nx, ny = x + dir[1], y + dir[2]
                if is_valid(nx, ny, grid) and grid[nx][ny] == grid[x][y] + 1 then
                    local new_path = {}
                    for _, p in ipairs(path) do
                        table.insert(new_path, p)
                    end
                    table.insert(new_path, {nx, ny})
                    table.insert(stack, {nx, ny, new_path})
                end
            end
        end
    end

    local unique_paths_count = 0
    for _ in pairs(all_paths) do
        unique_paths_count = unique_paths_count + 1
    end

    return unique_paths_count
end

local function compute_trailhead_ratings(grid)
    local total_rating = 0
    for i = 1, #grid do
        for j = 1, #grid[1] do
            if grid[i][j] == 0 then -- Found a trailhead
                total_rating = total_rating + find_trailhead_rating(grid, i, j)
            end
        end
    end
    return total_rating
end

local input_file = "input_level_10.txt"
local topographic_map = read_input(input_file)
local result = compute_trailhead_ratings(topographic_map)
print("Sum of the ratings of all trailheads:", result)
