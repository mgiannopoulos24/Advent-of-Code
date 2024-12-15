-- Parse the input file and initialize the grid and movements
function parse_input(filename)
    local file = io.open(filename, "r")
    local content = file:read("*all")
    file:close()

    local grid_raw, movements = content:match("^(.-)%s%s(.*)$")
    movements = movements:gsub("\n", "")
    local grid = {}
    for row in grid_raw:gmatch("[^\n]+") do
        local row_table = {}
        for char in row:gmatch(".") do
            table.insert(row_table, char)
        end
        table.insert(grid, row_table)
    end
    return grid, movements
end

-- Find the starting position of the player (@)
function find_starting_position(grid)
    for i = 1, #grid do
        for j = 1, #grid[i] do
            if grid[i][j] == "@" then
                return j, i
            end
        end
    end
    return 0, 0 -- Default position if not found
end

-- Process movements and update the grid
function process_movements(grid, movements, x, y)
    for mov in movements:gmatch(".") do
        if mov == "<" then
            x, y = move_left(grid, x, y)
        elseif mov == ">" then
            x, y = move_right(grid, x, y)
        elseif mov == "^" then
            x, y = move_up(grid, x, y)
        else -- mov == "v"
            x, y = move_down(grid, x, y)
        end
    end
end

-- Move the player and boxes to the left if possible
function move_left(grid, x, y)
    local box_count = 0
    for i = x - 1, 1, -1 do
        if grid[y][i] == "#" or grid[y][i] == "." then
            break
        end
        if grid[y][i] == "O" then
            box_count = box_count + 1
        end
    end

    local pos_x = x - box_count - 1
    if grid[y][pos_x] == "." then
        grid[y][x] = "."
        grid[y][pos_x] = "O"
        grid[y][x - 1] = "@"
        return x - 1, y
    end
    return x, y
end

-- Move the player and boxes to the right if possible
function move_right(grid, x, y)
    local box_count = 0
    for i = x + 1, #grid[y] do
        if grid[y][i] == "#" or grid[y][i] == "." then
            break
        end
        if grid[y][i] == "O" then
            box_count = box_count + 1
        end
    end

    local pos_x = x + box_count + 1
    if grid[y][pos_x] == "." then
        grid[y][x] = "."
        grid[y][pos_x] = "O"
        grid[y][x + 1] = "@"
        return x + 1, y
    end
    return x, y
end

-- Move the player and boxes up if possible
function move_up(grid, x, y)
    local box_count = 0
    for i = y - 1, 1, -1 do
        if grid[i][x] == "#" or grid[i][x] == "." then
            break
        end
        if grid[i][x] == "O" then
            box_count = box_count + 1
        end
    end

    local pos_y = y - box_count - 1
    if grid[pos_y][x] == "." then
        grid[y][x] = "."
        grid[pos_y][x] = "O"
        grid[y - 1][x] = "@"
        return x, y - 1
    end
    return x, y
end

-- Move the player and boxes down if possible
function move_down(grid, x, y)
    local box_count = 0
    for i = y + 1, #grid do
        if grid[i][x] == "#" or grid[i][x] == "." then
            break
        end
        if grid[i][x] == "O" then
            box_count = box_count + 1
        end
    end

    local pos_y = y + box_count + 1
    if grid[pos_y][x] == "." then
        grid[y][x] = "."
        grid[pos_y][x] = "O"
        grid[y + 1][x] = "@"
        return x, y + 1
    end
    return x, y
end

-- Calculate the score based on the positions of the boxes
function calculate_score(grid)
    local score = 0
    for y = 1, #grid do
        for x = 1, #grid[y] do
            if grid[y][x] == "O" then
                score = score + (y - 1) * 100 + (x - 1)
            end
        end
    end
    return score
end

function main()
    local filename = "input_level_15.txt"
    local grid, movements = parse_input(filename)
    local x, y = find_starting_position(grid)
    process_movements(grid, movements, x, y)
    local score = calculate_score(grid)
    print("Sum of all boxes' GPS coordinates:", score)
end

-- Run the main function
main()