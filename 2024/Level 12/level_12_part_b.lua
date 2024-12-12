local DefaultSetMap = {}
DefaultSetMap.__index = DefaultSetMap

function DefaultSetMap:new()
    local obj = { _map = {} }
    setmetatable(obj, self)
    return obj
end

function DefaultSetMap:get(key)
    if not self._map[key] then
        self._map[key] = {}
    end
    return self._map[key]
end

function DefaultSetMap:entries()
    return self._map
end

local function map_regions(grid)
    local rows = #grid
    local cols = #grid[1]
    local visited = {}
    local regions = {}

    local function bfs(start_r, start_c, char)
        local queue = {{start_r, start_c}}
        local region = {}

        while #queue > 0 do
            local r, c = table.unpack(table.remove(queue, 1))
            local key = r .. "," .. c
            if visited[key] then
                goto continue
            end

            visited[key] = true
            region[key] = true

            local directions = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}}
            for _, dir in ipairs(directions) do
                local dr, dc = table.unpack(dir)
                local nr, nc = r + dr, c + dc
                local neighbor_key = nr .. "," .. nc
                if nr >= 1 and nr <= rows and nc >= 1 and nc <= cols and grid[nr][nc] == char and not visited[neighbor_key] then
                    table.insert(queue, {nr, nc})
                end
            end
            ::continue::
        end

        return region
    end

    for r = 1, rows do
        for c = 1, cols do
            local key = r .. "," .. c
            if not visited[key] then
                local char = grid[r][c]
                local region = bfs(r, c, char)
                local boundaries = {DefaultSetMap:new(), DefaultSetMap:new(), DefaultSetMap:new(), DefaultSetMap:new()}
                table.insert(regions, {char, region, boundaries})
            end
        end
    end

    return regions
end

local function map_sides(grid, regions)
    local directions = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}}
    
    for _, region_data in ipairs(regions) do
        local char, region, boundaries = table.unpack(region_data)
        for pos, _ in pairs(region) do
            local r, c = pos:match("(%d+),(%d+)")
            r, c = tonumber(r), tonumber(c)
            
            for i, dir in ipairs(directions) do
                local dr, dc = table.unpack(dir)
                local nr, nc = r + dr, c + dc
                local sr, sc = r + dr / 2, c + dc / 2
                
                if nr < 1 or nr > #grid or nc < 1 or nc > #grid[1] or grid[r][c] ~= grid[nr][nc] then
                    if dr == 0 then -- Horizontal boundaries
                        if dc == 1 then
                            table.insert(boundaries[1]:get(sc), sr)
                        else
                            table.insert(boundaries[2]:get(sc), sr)
                        end
                    else -- Vertical boundaries
                        if dr == 1 then
                            table.insert(boundaries[3]:get(sr), sc)
                        else
                            table.insert(boundaries[4]:get(sr), sc)
                        end
                    end
                end
            end
        end
    end
end

local function count_sides(boundaries)
    local sides = 0
    for _, boundary in ipairs(boundaries) do
        for _, points in pairs(boundary:entries()) do
            table.sort(points)
            for i = 1, #points do
                if i == #points or points[i] + 1 ~= points[i + 1] then
                    sides = sides + 1
                end
            end
        end
    end
    return sides
end

local function table_length(tbl)
    local count = 0
    for _ in pairs(tbl) do count = count + 1 end
    return count
end

local function calculate_total_price(grid)
    local regions = map_regions(grid)
    map_sides(grid, regions)

    local total_price = 0
    for _, region_data in ipairs(regions) do
        local char, region, boundaries = table.unpack(region_data)
        local sides = count_sides(boundaries)
        total_price = total_price + sides * table_length(region)
    end

    return total_price
end

local function main()
    local input_file = "input_level_12.txt"
    local grid = {}
    
    for line in io.lines(input_file) do
        local row = {}
        for char in line:gmatch(".") do
            table.insert(row, char)
        end
        table.insert(grid, row)
    end

    local total_price = calculate_total_price(grid)
    print("Total price of fencing: " .. total_price)
end

main()