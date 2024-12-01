-- Function to read input from a file into two lists
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

-- Function to calculate the total distance between two sorted lists
local function calculate_total_distance(left, right)
    local total_distance = 0
    for i = 1, math.min(#left, #right) do
        total_distance = total_distance + math.abs(left[i] - right[i])
    end
    return total_distance
end

-- Main function
local function main()
    local filename = "input_level_1.txt"

    -- Read input from file
    local left, right = read_input(filename)
    if not left or not right then
        return
    end

    -- Sort both lists
    table.sort(left)
    table.sort(right)

    -- Calculate and display the total distance
    local total_distance = calculate_total_distance(left, right)
    print(string.format("Total distance: %d", total_distance))
end

-- Run the main function
main()
