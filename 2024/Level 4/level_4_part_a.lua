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

-- Counts occurrences of a word in the puzzle in all directions
local function search_word_in_puzzle(puzzle, word)
    local word_len = #word
    local rows = #puzzle
    local cols = #puzzle[1]
    local count = 0

    -- Helper function to check boundaries
    local function is_in_bounds(x, y)
        return x >= 1 and x <= rows and y >= 1 and y <= cols
    end

    -- All directions: (dx, dy)
    local directions = {
        {0, 1},   -- Horizontal right
        {0, -1},  -- Horizontal left
        {1, 0},   -- Vertical down
        {-1, 0},  -- Vertical up
        {1, 1},   -- Diagonal down-right
        {-1, -1}, -- Diagonal up-left
        {1, -1},  -- Diagonal down-left
        {-1, 1},  -- Diagonal up-right
    }

    -- Search for the word
    for x = 1, rows do
        for y = 1, cols do
            for _, dir in ipairs(directions) do
                local dx, dy = dir[1], dir[2]
                local found = true
                for k = 0, word_len - 1 do
                    local nx, ny = x + k * dx, y + k * dy
                    if not is_in_bounds(nx, ny) or puzzle[nx]:sub(ny, ny) ~= word:sub(k + 1, k + 1) then
                        found = false
                        break
                    end
                end
                if found then
                    count = count + 1
                end
            end
        end
    end

    return count
end

-- Main function
local function main()
    -- Read the puzzle input
    local puzzle = read_puzzle(file_path)
    
    -- Word to search for
    local word = "XMAS"
    
    -- Count occurrences of the word
    local occurrences = search_word_in_puzzle(puzzle, word)
    print(string.format("The word '%s' appears %d times in the puzzle.", word, occurrences))
end

-- Run the main function
main()
