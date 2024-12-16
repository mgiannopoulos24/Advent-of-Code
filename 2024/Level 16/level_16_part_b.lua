local heap = {}
local directions = {
    {1, 0},  -- East (1)
    {0, 1},  -- South (2)
    {-1, 0}, -- West (3)
    {0, -1}  -- North (4)
}  -- Directions are now 1-based

-- Priority Queue (min-heap) implementation
local function heap_push(heap, score, x, y, direction)
    table.insert(heap, {score, x, y, direction})
    local i = #heap
    while i > 1 do
        local parent = math.floor(i / 2)
        if heap[i][1] < heap[parent][1] then
            heap[i], heap[parent] = heap[parent], heap[i]
            i = parent
        else
            break
        end
    end
end

local function heap_pop(heap)
    if #heap == 0 then return nil end
    local root = heap[1]
    heap[1] = heap[#heap]
    table.remove(heap)
    local i = 1
    while true do
        local left = i * 2
        local right = i * 2 + 1
        local smallest = i
        if left <= #heap and heap[left][1] < heap[smallest][1] then
            smallest = left
        end
        if right <= #heap and heap[right][1] < heap[smallest][1] then
            smallest = right
        end
        if smallest == i then break end
        heap[i], heap[smallest] = heap[smallest], heap[i]
        i = smallest
    end
    return root
end

local function parse_input(file_path)
    local maze = {}
    for line in io.lines(file_path) do
        local row = {}
        for i = 1, #line do
            table.insert(row, line:sub(i, i))
        end
        table.insert(maze, row)
    end
    return maze
end

local function find_start_and_end(maze)
    local start, end_pos = nil, nil
    for y, row in ipairs(maze) do
        for x, cell in ipairs(row) do
            if cell == 'S' then
                start = {x, y}
            elseif cell == 'E' then
                end_pos = {x, y}
            end
        end
    end
    return start, end_pos
end

local function is_valid_move(maze, x, y)
    return y > 0 and x > 0 and y <= #maze and x <= #maze[1] and maze[y][x] ~= '#'
end

local function flip_direction(direction)
    return ((direction + 1) % 4) + 1  -- Adjusted for 1-based indexing
end

local function dijkstra(maze, starts)
    local pq = {}
    local distances = {}

    for _, start in ipairs(starts) do
        heap_push(pq, 0, start[1], start[2], start[3])
        distances[start[1] .. "," .. start[2] .. "," .. start[3]] = 0
    end

    while #pq > 0 do
        local score, x, y, direction = table.unpack(heap_pop(pq))
        local key = x .. "," .. y .. "," .. direction

        if score > (distances[key] or math.huge) then
            goto continue
        end

        -- Move forward
        local dx, dy = directions[direction][1], directions[direction][2]
        local nx, ny = x + dx, y + dy
        if is_valid_move(maze, nx, ny) then
            local new_score = score + 1
            local new_key = nx .. "," .. ny .. "," .. direction
            if new_score < (distances[new_key] or math.huge) then
                distances[new_key] = new_score
                heap_push(pq, new_score, nx, ny, direction)
            end
        end

        -- Rotate left and right
        for _, rotation in ipairs({-1, 1}) do
            local new_direction = (direction + rotation)
            if new_direction < 1 then
                new_direction = new_direction + 4
            elseif new_direction > 4 then
                new_direction = new_direction - 4
            end
            local new_score = score + 1000
            local new_key = x .. "," .. y .. "," .. new_direction
            if new_score < (distances[new_key] or math.huge) then
                distances[new_key] = new_score
                heap_push(pq, new_score, x, y, new_direction)
            end
        end

        ::continue::
    end

    return distances
end

local function solve_maze(file_path)
    local maze = parse_input(file_path)
    local start, end_pos = find_start_and_end(maze)

    if not start or not end_pos then
        print("Start or end position not found in the maze.")
        return
    end

    -- Run Dijkstra from start
    local from_start = dijkstra(maze, {
        {start[1], start[2], 1},
        {start[1], start[2], 2},
        {start[1], start[2], 3},
        {start[1], start[2], 4},
    })

    -- Run Dijkstra from end
    local from_end = dijkstra(maze, {
        {end_pos[1], end_pos[2], 1},
        {end_pos[1], end_pos[2], 2},
        {end_pos[1], end_pos[2], 3},
        {end_pos[1], end_pos[2], 4},
    })

    -- Find shortest path
    local shortest_path = math.huge
    for d = 1, 4 do
        local key = end_pos[1] .. "," .. end_pos[2] .. "," .. d
        if from_start[key] and from_start[key] < shortest_path then
            shortest_path = from_start[key]
        end
    end

    if shortest_path == math.huge then
        print("No valid path found.")
        return
    end

    -- Find tiles in best paths
    local tiles_in_best_paths = {}
    for key, dist_start in pairs(from_start) do
        local parts = {}
        for part in string.gmatch(key, "([^,]+)") do
            table.insert(parts, part)
        end
        local x, y, direction = tonumber(parts[1]), tonumber(parts[2]), tonumber(parts[3])
        local flipped_dir = flip_direction(direction)
        local end_key = x .. "," .. y .. "," .. flipped_dir
        local dist_end = from_end[end_key] or math.huge
        if dist_start + dist_end == shortest_path then
            tiles_in_best_paths[x .. "," .. y] = true
        end
    end

    local count = 0
    for _ in pairs(tiles_in_best_paths) do
        count = count + 1
    end

    print("Lowest score: " .. shortest_path)
    print("Number of tiles in at least one best path: " .. count)
end

local file_path = "input_level_16.txt"
solve_maze(file_path)