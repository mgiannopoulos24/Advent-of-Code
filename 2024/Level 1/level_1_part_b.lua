-- Function to read input from a file into two tables
local function read_input(filename)
    local left = {}
    local right = {}
    local file, err = io.open(filename, "r")
    if not file then
        print("Error opening file: " .. err)
        return nil, nil
    end

    for line in file:lines() do
        local l, r = line:match("(%d+)%s+(%d+)")
        if l and r then
            table.insert(left, tonumber(l))
            table.insert(right, tonumber(r))
        end
    end

    file:close()
    return left, right
end

-- Function to calculate the frequency of elements in an array
local function calculate_frequency(array)
    local freq = {}
    local min_val, max_val = array[1], array[1]

    -- Determine the range of the values in the array
    for _, value in ipairs(array) do
        min_val = math.min(min_val, value)
        max_val = math.max(max_val, value)
    end

    -- Populate the frequency table
    for _, value in ipairs(array) do
        freq[value] = (freq[value] or 0) + 1
    end

    return freq, min_val, max_val
end

-- Function to calculate the similarity score
local function calculate_similarity_score(left, right)
    local freq_right, min_val, max_val = calculate_frequency(right)

    -- Calculate similarity score
    local similarity_score = 0
    for _, value in ipairs(left) do
        if value >= min_val and value <= max_val then
            similarity_score = similarity_score + (value * (freq_right[value] or 0))
        end
    end

    return similarity_score
end

-- Main function
local function main()
    local filename = "input_level_1.txt"

    -- Read input from file
    local left, right = read_input(filename)
    if not left or not right then
        return
    end

    -- Compute and print the similarity score
    local similarity_score = calculate_similarity_score(left, right)
    print(string.format("Similarity score: %d", similarity_score))
end

-- Run the main function
main()
