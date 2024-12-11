-- Function to read the input file and return the initial arrangement of stones
function read_input()
    local file = io.open("input_level_11.txt", "r")
    local line = file:read("*line")
    file:close()
    local stones = {}
    for num in string.gmatch(line, "%S+") do
        table.insert(stones, tonumber(num))
    end
    return stones
end

-- Function to simulate a single blink and return the new arrangement of stones
function blink(stones)
    local new_stones = {}
    for _, stone in ipairs(stones) do
        if stone == 0 then
            table.insert(new_stones, 1)
        elseif #tostring(stone) % 2 == 0 then
            -- Even number of digits
            local digits = tostring(stone)
            local mid = math.floor(#digits / 2)
            local left = tonumber(string.sub(digits, 1, mid))
            local right = tonumber(string.sub(digits, mid + 1))
            table.insert(new_stones, left)
            table.insert(new_stones, right)
        else
            table.insert(new_stones, stone * 2024)
        end
    end
    return new_stones
end

-- Function to simulate the specified number of blinks starting with the initial stones
function simulate_blinks(initial_stones, blinks)
    local stones = initial_stones
    for _ = 1, blinks do
        stones = blink(stones)
    end
    return stones
end

local function main()

    local initial_stones = read_input()

    local blinks = 25

    local final_stones = simulate_blinks(initial_stones, blinks)

    print("Number of stones after 25 blinks:",#final_stones)
end

main()
