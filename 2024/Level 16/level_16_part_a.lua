function parse_input(file_path)
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

function find_start_and_end(maze)
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

function is_valid_move(maze, x, y)
    return y > 0 and x > 0 and y <= #maze and x <= #maze[1] and maze[y][x] ~= '#'
end

-- Priority Queue (min-heap) implementation
function heap_push(heap, score, x, y, direction)
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

function heap_pop(heap)
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

function solve_maze(file_path)
    local maze = parse_input(file_path)
    local start, end_pos = find_start_and_end(maze)

    local directions = {{1, 0}, {0, 1}, {-1, 0}, {0, -1}}  -- East, South, West, North
    local pq = {}
    heap_push(pq, 0, start[1], start[2], 1)  -- Start with East direction
    local visited = {}

    while #pq > 0 do
        local score, x, y, direction = table.unpack(heap_pop(pq))

        if x == end_pos[1] and y == end_pos[2] then
            return score
        end

        if visited[x] and visited[x][y] and visited[x][y][direction] then
            goto continue
        end
        visited[x] = visited[x] or {}
        visited[x][y] = visited[x][y] or {}
        visited[x][y][direction] = true

        -- Try moving forward
        local nx, ny = x + directions[direction][1], y + directions[direction][2]
        if is_valid_move(maze, nx, ny) then
            heap_push(pq, score + 1, nx, ny, direction)
        end

        -- Try rotating (left or right)
        for _, rotation in ipairs({-1, 1}) do
            local new_direction = (direction + rotation - 1) % 4 + 1
            heap_push(pq, score + 1000, x, y, new_direction)
        end

        ::continue::
    end

    return -1  -- No valid path
end

local file_path = "input_level_16.txt"  
local result = solve_maze(file_path)
print("Lowest score: " .. result)
