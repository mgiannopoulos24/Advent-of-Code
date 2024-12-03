function sum_enabled_mul_results(file_path)
    local total = 0
    local mul_enabled = true -- Initially, mul instructions are enabled

    -- Read the entire file
    local file = io.open(file_path, "r")
    if not file then
        print("Error: Unable to open file " .. file_path)
        return nil
    end

    local data = file:read("*all")
    file:close()

    -- Patterns for instructions
    local mul_pattern = "mul%((%d+),(%d+)%)"
    local do_pattern = "do%(%s*%)"
    local dont_pattern = "don't%(%s*%)"

    -- Iterate through the data
    local pos = 1
    while pos <= #data do
        local do_start, do_end = string.find(data, do_pattern, pos)
        local dont_start, dont_end = string.find(data, dont_pattern, pos)
        local mul_start, mul_end, x, y = string.find(data, mul_pattern, pos)

        -- Determine which instruction is next
        local next_event = {}
        if do_start then table.insert(next_event, {type = "do", start = do_start, end_ = do_end}) end
        if dont_start then table.insert(next_event, {type = "don't", start = dont_start, end_ = dont_end}) end
        if mul_start then table.insert(next_event, {type = "mul", start = mul_start, end_ = mul_end, x = x, y = y}) end

        table.sort(next_event, function(a, b) return a.start < b.start end)

        if #next_event == 0 then
            break
        end

        local event = next_event[1]
        if event.type == "do" then
            mul_enabled = true
            pos = event.end_ + 1
        elseif event.type == "don't" then
            mul_enabled = false
            pos = event.end_ + 1
        elseif event.type == "mul" then
            if mul_enabled then
                total = total + (tonumber(event.x) * tonumber(event.y))
            end
            pos = event.end_ + 1
        end
    end

    return total
end

-- Main
local input_file = "input_level_3.txt" -- Replace with the path to your input file
local result = sum_enabled_mul_results(input_file)
if result then
    print("Sum of enabled mul results: " .. result)
end
