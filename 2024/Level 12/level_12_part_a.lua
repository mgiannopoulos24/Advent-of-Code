local function parse_input(file_path)
    local grid = {}
    for line in io.lines(file_path) do
        table.insert(grid, {})
        for char in line:gmatch(".") do
            table.insert(grid[#grid], char)
        end
    end
    return grid
end

local function calculate_region_properties(grid, x, y, visited)
    local stack = {{x, y}}
    local plant_type = grid[x][y]
    local area = 0
    local perimeter = 0
    visited[x .. "," .. y] = true

    while #stack > 0 do
        local cx, cy = table.unpack(table.remove(stack))
        area = area + 1
        -- Check all 4 neighbors
        for _, dir in ipairs({{-1, 0}, {1, 0}, {0, -1}, {0, 1}}) do
            local dx, dy = dir[1], dir[2]
            local nx, ny = cx + dx, cy + dy
            if grid[nx] and grid[nx][ny] then
                if grid[nx][ny] == plant_type then
                    if not visited[nx .. "," .. ny] then
                        visited[nx .. "," .. ny] = true
                        table.insert(stack, {nx, ny})
                    end
                else
                    perimeter = perimeter + 1 -- Neighbor is a different type
                end
            else
                perimeter = perimeter + 1 -- Neighbor is out of bounds
            end
        end
    end

    return area, perimeter
end

local function calculate_total_fencing_cost(file_path)
    local grid = parse_input(file_path)
    local visited = {}
    local total_cost = 0

    for x = 1, #grid do
        for y = 1, #grid[1] do
            if not visited[x .. "," .. y] then
                -- Calculate the properties of the new region
                local area, perimeter = calculate_region_properties(grid, x, y, visited)
                -- Calculate the cost for this region
                total_cost = total_cost + area * perimeter
            end
        end
    end

    return total_cost
end

local input_file ="input_level_12.txt" 
local result = calculate_total_fencing_cost(input_file)
print("Total fencing cost: " .. result)