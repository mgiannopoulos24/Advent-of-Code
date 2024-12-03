function sum_valid_mul_results(file_path)
    local total = 0

    -- Read the entire file
    local file = io.open(file_path, "r")
    if not file then
        print("Error: Unable to open file " .. file_path)
        return nil
    end

    local data = file:read("*all")
    file:close()

    -- Pattern to match valid mul(X,Y)
    local pattern = "mul%((%d+),(%d+)%)"

    -- Iterate through all matches
    for x, y in string.gmatch(data, pattern) do
        total = total + (tonumber(x) * tonumber(y))
    end

    return total
end

-- Main
local input_file = "input_level_3.txt" -- Replace with the path to your input file
local result = sum_valid_mul_results(input_file)
if result then
    print("Sum of valid mul results: " .. result)
end