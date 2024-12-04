-- File path 
local file_path = "input_level_4.txt"

-- Reads the word search puzzle from the input file
local function read_puzzle(file_path)
    local puzzle = {}
    for line in io.lines(file_path) do
        table.insert(puzzle, line)
    end
    return puzzle
end

-- Checks if an X-MAS pattern exists with center at (x, y)
local function is_xmas_at(puzzle, x, y)
    -- Define the relative positions for NW-SE and NE-SW diagonals
    local diagonals = {
        NW_SE = {{-1, -1}, {0, 0}, {1, 1}},
        NE_SW = {{-1, 1}, {0, 0}, {1, -1}}
    }

    local function get_sequence(positions)
        local sequence = {}
        for _, pos in ipairs(positions) do
            local dx, dy = pos[1], pos[2]
            local nx, ny = x + dx, y + dy
            -- Check if the position is within bounds
            if nx >= 1 and nx <= #puzzle and ny >= 1 and ny <= #puzzle[1] then
                table.insert(sequence, puzzle[nx]:sub(ny, ny))
            else
                return nil -- Out of bounds
            end
        end
        return table.concat(sequence)
    end

    local nw_se = get_sequence(diagonals.NW_SE)
    local ne_sw = get_sequence(diagonals.NE_SW)

    -- Check if both diagonals contain 'MAS' or 'SAM'
    local valid_nw_se = nw_se == "MAS" or nw_se == "SAM"
    local valid_ne_sw = ne_sw == "MAS" or ne_sw == "SAM"

    return valid_nw_se and valid_ne_sw
end

-- Counts the number of X-MAS patterns in the puzzle
local function count_xmas(puzzle)
    local rows, cols = #puzzle, #puzzle[1]
    local count = 0

    -- Iterate over each possible center position
    for x = 2, rows - 1 do
        for y = 2, cols - 1 do
            if puzzle[x]:sub(y, y) == "A" and is_xmas_at(puzzle, x, y) then
                count = count + 1
            end
        end
    end

    return count
end

-- Main function
local function main()
    -- Read the puzzle input
    local puzzle = read_puzzle(file_path)

    -- Count occurrences of the X-MAS pattern
    local xmas_count = count_xmas(puzzle)
    print(string.format("The number of X-MAS patterns in the puzzle is %d.", xmas_count))
end

-- Run the main function
main()
