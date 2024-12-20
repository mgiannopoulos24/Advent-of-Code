local function read_input(filename)
    local grid = {}
    for line in io.lines(filename) do
        table.insert(grid, {})
        for char in line:gmatch(".") do
            table.insert(grid[#grid], char)
        end
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

    local queue = {}
    local sx, sy = start[1], start[2]
    T[sx][sy] = 0
    table.insert(queue, {sx, sy})

    while #queue > 0 do
        local current = table.remove(queue, 1)
        local x, y = current[1], current[2]

        for _, direction in ipairs({{-1, 0}, {1, 0}, {0, -1}, {0, 1}}) do
            local dx, dy = direction[1], direction[2]
            local nx, ny = x + dx, y + dy
            if nx >= 1 and nx <= rows and ny >= 1 and ny <= cols then
                if grid[nx][ny] ~= "#" and T[nx][ny] > T[x][y] + 1 then
                    T[nx][ny] = T[x][y] + 1
                    table.insert(queue, {nx, ny})
                end
            end
        end
    end

    return T
end

local function main()
    local grid = read_input("input_level_20.txt")
    local rows = #grid
    local cols = #grid[1]
    local S, E

    for i = 1, rows do
        for j = 1, cols do
            if grid[i][j] == "S" then
                S = {i, j}
                grid[i][j] = "."
            end
            if grid[i][j] == "E" then
                E = {i, j}
                grid[i][j] = "."
            end
        end
    end

    local T_no_cheat = bfs(grid, S)
    local T_no_cheat_rev = bfs(grid, E)
    local T_normal = T_no_cheat[E[1]][E[2]]

    local cheats_set = {}
    local count = 0

    for i = 1, rows do
        for j = 1, cols do
            if grid[i][j] == "." then
                local start_cheat_pos = {i, j}

                for _, direction1 in ipairs({{-1, 0}, {1, 0}, {0, -1}, {0, 1}}) do
                    local dx1, dy1 = direction1[1], direction1[2]
                    local x1, y1 = i + dx1, j + dy1

                    if x1 >= 1 and x1 <= rows and y1 >= 1 and y1 <= cols then
                        if grid[x1][y1] == "." then
                            local cheat_length = 1
                            local end_cheat_pos = {x1, y1}
                            local T_cheat = T_no_cheat[i][j] + cheat_length + T_no_cheat_rev[x1][y1]
                            local Time_saved = T_normal - T_cheat

                            if Time_saved >= 100 then
                                local cheat_id = tostring(start_cheat_pos[1]) .. "," .. tostring(start_cheat_pos[2]) .. ";" .. tostring(end_cheat_pos[1]) .. "," .. tostring(end_cheat_pos[2])
                                if not cheats_set[cheat_id] then
                                    cheats_set[cheat_id] = true
                                    count = count + 1
                                end
                            end
                        elseif grid[x1][y1] == "#" then
                            for _, direction2 in ipairs({{-1, 0}, {1, 0}, {0, -1}, {0, 1}}) do
                                local dx2, dy2 = direction2[1], direction2[2]
                                local x2, y2 = x1 + dx2, y1 + dy2

                                if x2 >= 1 and x2 <= rows and y2 >= 1 and y2 <= cols then
                                    if grid[x2][y2] == "." then
                                        local cheat_length = 2
                                        local end_cheat_pos = {x2, y2}
                                        local T_cheat = T_no_cheat[i][j] + cheat_length + T_no_cheat_rev[x2][y2]
                                        local Time_saved = T_normal - T_cheat

                                        if Time_saved >= 100 then
                                            local cheat_id = tostring(start_cheat_pos[1]) .. "," .. tostring(start_cheat_pos[2]) .. ";" .. tostring(end_cheat_pos[1]) .. "," .. tostring(end_cheat_pos[2])
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
            end
        end
    end

    print("Number of cheats that save at least 100 picoseconds:", count)
end

main()
