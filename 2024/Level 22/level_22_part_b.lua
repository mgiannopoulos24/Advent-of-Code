-- Function to compute the next secret number in the sequence
local function next_secret_number(secret)
    -- Step 1: Multiply by 64, mix, and prune
    secret = secret ~ (secret * 64)
    secret = secret % 16777216

    -- Step 2: Divide by 32, round down, mix, and prune
    secret = secret ~ math.floor(secret / 32)
    secret = secret % 16777216

    -- Step 3: Multiply by 2048, mix, and prune
    secret = secret ~ (secret * 2048)
    secret = secret % 16777216

    return secret
end

local function sequence_to_int(seq)
    local result = 0
    for i = 1, 4 do
        result = result * 19
        result = result + seq[i] + 9
    end
    return result
end

local function main()

    local initial_secrets = {}
    for line in io.lines("input_level_22.txt") do
        table.insert(initial_secrets, tonumber(line))
    end

    -- Initialize sequence totals
    local sequence_total = {}
    for i = 1, 19 ^ 4 do
        sequence_total[i] = 0
    end

    for _, secret in ipairs(initial_secrets) do
        local previous = 0
        local values = {}
        local changes = {}
        local seen_sequence = {}

        for i = 1, 19 ^ 4 do
            seen_sequence[i] = 0
        end

        -- Generate values and changes for 2000 iterations
        for i = 1, 2000 do
            secret = next_secret_number(secret)
            values[i] = secret % 10
            changes[i] = (secret % 10) - previous
            previous = secret % 10
        end

        -- Process sequences and update sequence totals
        for i = 2, 1996 do -- Ensure i+3 <= 2000
            local sequence_num = sequence_to_int({changes[i], changes[i + 1], changes[i + 2], changes[i + 3]})
            if seen_sequence[sequence_num] == 0 then
                seen_sequence[sequence_num] = 1
                sequence_total[sequence_num] = sequence_total[sequence_num] + values[i + 3]
            end
        end
    end

    -- Find the maximum bananas
    local bananas = 0
    for _, total in ipairs(sequence_total) do
        if total > bananas then
            bananas = total
        end
    end

    print("The maximum number of bananas you can get is:", bananas)
end

main()
