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

-- Function to compute the 2000th secret number for a given initial secret
local function compute_2000th_secret(initial_secret)
    local secret = initial_secret
    for _ = 1, 2000 do
        secret = next_secret_number(secret)
    end
    return secret
end

local function main()

    local initial_secrets = {}
    for line in io.lines("input_level_22.txt") do
        table.insert(initial_secrets, tonumber(line))
    end

    -- Compute the 2000th secret number for each buyer
    local results = {}
    for _, secret in ipairs(initial_secrets) do
        table.insert(results, compute_2000th_secret(secret))
    end

    -- Calculate the sum of all 2000th secret numbers
    local total_sum = 0
    for _, result in ipairs(results) do
        total_sum = total_sum + result
    end

    print("The sum of the 2000th secret numbers is: " .. total_sum)
end

main()
