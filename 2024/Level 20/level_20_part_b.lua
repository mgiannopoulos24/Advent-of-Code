local function read_input(filename)
    local grid = {}
    for line in io.lines(filename) do
        local row = {}
        for char in line:gmatch('.') do
            table.insert(row, char)
        end
        table.insert(grid, row)
    end
    return grid
end

local function bfs(grid, start)
    local rows = #grid
    local cols = #grid[1]
    local T = {}
    for i = 1, rows do
        T[i] = {}
        for j = 1, cols do
            T[i][j] = math.huge
        end
    end
    local queue = {{start[1], start[2]}}
    T[start[1]][start[2]] = 0

    while #queue > 0 do
        local x, y = table.unpack(table.remove(queue, 1))
        for _, direction in ipairs({{-1, 0}, {1, 0}, {0, -1}, {0, 1}}) do
            local nx, ny = x + direction[1], y + direction[2]
            if nx > 0 and nx <= rows and ny > 0 and ny <= cols and grid[nx][ny] ~= '#' and T[nx][ny] > T[x][y] + 1 then
                T[nx][ny] = T[x][y] + 1
                table.insert(queue, {nx, ny})
            end
        end
    end
    return T
end

local function bfs_cheat(grid, start, max_cheat_length)
    local rows = #grid
    local cols = #grid[1]
    local cheat_T = {}
    for i = 1, rows do
        cheat_T[i] = {}
        for j = 1, cols do
            cheat_T[i][j] = -1
        end
    end
    local queue = {{start[1], start[2]}}
    cheat_T[start[1]][start[2]] = 0

    while #queue > 0 do
        local x, y = table.unpack(table.remove(queue, 1))
        if cheat_T[x][y] == max_cheat_length then
            goto continue
        end
        for _, direction in ipairs({{-1, 0}, {1, 0}, {0, -1}, {0, 1}}) do
            local nx, ny = x + direction[1], y + direction[2]
            if nx > 0 and nx <= rows and ny > 0 and ny <= cols and cheat_T[nx][ny] == -1 then
                cheat_T[nx][ny] = cheat_T[x][y] + 1
                table.insert(queue, {nx, ny})
            end
        end
        ::continue::
    end
    return cheat_T
end

local function main()
    local grid = read_input('input_level_20.txt')
    local rows = #grid
    local cols = #grid[1]
    local S, E

    for i = 1, rows do
        for j = 1, cols do
            if grid[i][j] == 'S' then
                S = {i, j}
                grid[i][j] = '.'
            elseif grid[i][j] == 'E' then
                E = {i, j}
                grid[i][j] = '.'
            end
        end
    end

    local T_no_cheat = bfs(grid, S)
    local T_no_cheat_rev = bfs(grid, E)
    local T_normal = T_no_cheat[E[1]][E[2]]

    local max_cheat_length = 20
    local cheats_set = {}
    local count = 0

    for i = 1, rows do
        for j = 1, cols do
            if grid[i][j] == '.' then
                local start_cheat_pos = {i, j}
                local cheat_T = bfs_cheat(grid, start_cheat_pos, max_cheat_length)
                for x = 1, rows do
                    for y = 1, cols do
                        if grid[x][y] == '.' and cheat_T[x][y] ~= -1 and cheat_T[x][y] <= max_cheat_length and (x ~= i or y ~= j) then
                            local end_cheat_pos = {x, y}
                            local cheat_length = cheat_T[x][y]
                            local T_cheat = T_no_cheat[i][j] + cheat_length + T_no_cheat_rev[x][y]
                            local Time_saved = T_normal - T_cheat
                            if Time_saved >= 100 then
                                local cheat_id = table.concat(start_cheat_pos, ',') .. '-' .. table.concat(end_cheat_pos, ',')
                                if not cheats_set[cheat_id] then
                                    cheats_set[cheat_id] = true
                                    count = count + 1
                                end
                            end
                        end
                    end
                end
            end
        end
    end

    print("Number of cheats that save at least 100 picoseconds:", count)
end

main()

-- 42 seconds