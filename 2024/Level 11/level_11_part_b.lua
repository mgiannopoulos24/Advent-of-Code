-- Function to read the input file and return the initial arrangement of stones as a frequency table
function read_input()
    local file = io.open("input_level_11.txt", "r")
    local line = file:read("*line")
    file:close()
    local stones = {}
    for num in string.gmatch(line, "%S+") do
        table.insert(stones, tonumber(num))
    end

    local stone_counts = {}
    for _, stone in ipairs(stones) do
        if stone_counts[stone] then
            stone_counts[stone] = stone_counts[stone] + 1
        else
            stone_counts[stone] = 1
        end
    end
    return stone_counts
end

-- Function to simulate a single blink and return the new frequency table of stones
function blink(stone_counts)
    local new_stone_counts = {}
    for stone, count in pairs(stone_counts) do
        if stone == 0 then
            new_stone_counts[1] = (new_stone_counts[1] or 0) + count
        else
            local num_digits = #tostring(stone)
            if num_digits % 2 == 0 then
                -- Even number of digits
                local digits = tostring(stone)
                local mid = math.floor(num_digits / 2)
                local left = tonumber(string.sub(digits, 1, mid))
                local right = tonumber(string.sub(digits, mid + 1))
                new_stone_counts[left] = (new_stone_counts[left] or 0) + count
                new_stone_counts[right] = (new_stone_counts[right] or 0) + count
            else
                -- Odd number of digits
                new_stone_counts[stone * 2024] = (new_stone_counts[stone * 2024] or 0) + count
            end
        end
    end
    return new_stone_counts
end

-- Function to simulate the specified number of blinks starting with the initial stone counts
function simulate_blinks(initial_stone_counts, blinks)
    local stone_counts = initial_stone_counts
    for _ = 1, blinks do
        stone_counts = blink(stone_counts)
    end
    return stone_counts
end

local function main()

    local initial_stone_counts = read_input()

    local blinks = 75

    local final_stone_counts = simulate_blinks(initial_stone_counts, blinks)

    local total_stones = 0
    for _, count in pairs(final_stone_counts) do
        total_stones = total_stones + count
    end

    print("Number of stones after 75 blinks:",total_stones)
end

main()
