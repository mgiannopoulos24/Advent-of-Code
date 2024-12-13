local function calculate_min_tokens(input_file)
    -- Read and parse input file
    local file = io.open(input_file, "r")
    local data = file:read("*all"):gsub("\r\n", "\n"):gsub("\r", "\n"):gsub("\n\n", "\1")
    file:close()

    local machines = {}
    for machine in data:gmatch("[^\1]+") do
        table.insert(machines, machine)
    end

    local results = {}

    for _, machine in ipairs(machines) do
        local lines = {}
        for line in machine:gmatch("[^\n]+") do
            table.insert(lines, line)
        end

        local a_x, a_y = lines[1]:match("X%+(%d+), Y%+(%d+)")
        local b_x, b_y = lines[2]:match("X%+(%d+), Y%+(%d+)")
        local prize_x, prize_y = lines[3]:match("X=(%d+), Y=(%d+)")

        a_x, a_y = tonumber(a_x), tonumber(a_y)
        b_x, b_y = tonumber(b_x), tonumber(b_y)
        prize_x, prize_y = tonumber(prize_x), tonumber(prize_y)

        local min_tokens = math.huge
        local won = false

        -- Try all combinations of button presses within the limit
        for a_presses = 0, 100 do
            for b_presses = 0, 100 do
                local x = a_presses * a_x + b_presses * b_x
                local y = a_presses * a_y + b_presses * b_y

                if x == prize_x and y == prize_y then
                    local tokens = a_presses * 3 + b_presses * 1
                    min_tokens = math.min(min_tokens, tokens)
                    won = true
                end
            end
        end

        if won then
            table.insert(results, min_tokens)
        end
    end

    -- Calculate total tokens for all prizes won
    local total_tokens = 0
    for _, tokens in ipairs(results) do
        total_tokens = total_tokens + tokens
    end

    return #results, total_tokens
end

local input_file = "input_level_13.txt"

local prizes_won, total_tokens = calculate_min_tokens(input_file)

print("Prizes won: " .. prizes_won)
print("Total tokens spent: " .. total_tokens)
