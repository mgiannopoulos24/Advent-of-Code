-- Function to evaluate an expression given numbers and operators (strictly left-to-right)
local function evaluate_expression(nums, ops)
    local result = nums[1]
    for i = 1, #ops do
        if ops[i] == "+" then
            result = result + nums[i + 1]
        elseif ops[i] == "*" then
            result = result * nums[i + 1]
        elseif ops[i] == "||" then
            -- Concatenate result and nums[i + 1] as strings, then convert back to an integer
            result = tonumber(tostring(result) .. tostring(nums[i + 1]))
        end
    end
    return result
end

-- Function to generate all sequences of '+', '*', and '||' for num_slots
local function generate_operators(num_slots)
    local results = {}
    local total_combinations = 3 ^ num_slots
    for i = 0, total_combinations - 1 do
        local combo = {}
        local n = i
        for _ = 1, num_slots do
            local op_index = (n % 3) + 1
            n = math.floor(n / 3)
            if op_index == 1 then
                table.insert(combo, "+")
            elseif op_index == 2 then
                table.insert(combo, "*")
            elseif op_index == 3 then
                table.insert(combo, "||")
            end
        end
        table.insert(results, combo)
    end
    return results
end

-- Function to check if a target value can be formed using the given numbers
local function can_form_value(target, nums)
    -- If there's only one number, just check if it's equal to the target
    if #nums == 1 then
        return nums[1] == target
    end

    -- Try all combinations of operators
    local operators = generate_operators(#nums - 1)
    for _, ops in ipairs(operators) do
        local val = evaluate_expression(nums, ops)
        if val == target then
            return true
        end
    end
    return false
end

-- Main function
local function main()
    local total_sum = 0

    local file = io.open("input_level_7.txt", "r")
    if not file then
        print("Failed to open input.txt")
        return
    end

    for line in file:lines() do
        line = line:match("^%s*(.-)%s*$") -- Trim whitespace
        if line ~= "" then
          
            local target_str, nums_str = line:match("^(.-):%s*(.+)$")
            local target = tonumber(target_str)
            local nums = {}
            for num in nums_str:gmatch("%S+") do
                table.insert(nums, tonumber(num))
            end

            -- Check if we can form the target with some combination of +, *, and ||
            if can_form_value(target, nums) then
                total_sum = total_sum + target
            end
        end
    end

    file:close()
    print(total_sum)
end

main()
