local function parse_coordinates(line)
    local x, y = line:match("X%+(%d+), Y%+(%d+)")
    return tonumber(x), tonumber(y)
end

local function parse_prize_coordinates(line)
    local x, y = line:match("X=(%d+), Y=(%d+)")
    return tonumber(x) + 10000000000000, tonumber(y) + 10000000000000
end

local function calculate_min_tokens(input_file)
    local file = io.open(input_file, "r")
    local lines = {}
    for line in file:lines() do
        table.insert(lines, line)
    end
    file:close()

    local prizes_won = 0
    local total_tokens = 0

    local i = 1
    while i <= #lines do
        -- Parse machine A, machine B, and prize lines
        local a_x, a_y = parse_coordinates(lines[i]:gsub("%s+$", ""))
        i = i + 1
        local b_x, b_y = parse_coordinates(lines[i]:gsub("%s+$", ""))
        i = i + 1
        local prize_x, prize_y = parse_prize_coordinates(lines[i]:gsub("%s+$", ""))
        i = i + 2 -- Skip empty line

        local min_tokens = math.huge
        local won = false

        -- Optimized approach using Diophantine equations
        local det = a_x * b_y - b_x * a_y
        if det ~= 0 then
            local dx = prize_x * b_y - prize_y * b_x
            local dy = prize_y * a_x - prize_x * a_y

            if dx % det == 0 and dy % det == 0 then
                local a_presses = dx // det
                local b_presses = dy // det

                if a_presses >= 0 and b_presses >= 0 then
                    local tokens = a_presses * 3 + b_presses * 1
                    if tokens < min_tokens then
                        min_tokens = tokens
                        won = true
                    end
                end
            end
        end

        if won then
            prizes_won = prizes_won + 1
            total_tokens = total_tokens + min_tokens
        end
    end

    print("Prizes won: " .. prizes_won)
    print("Total tokens spent: " .. total_tokens)
end

-- Input file path
local input_file = "input_level_13.txt"

calculate_min_tokens(input_file)