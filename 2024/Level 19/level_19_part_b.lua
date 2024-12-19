function read_input(file_path)
    local towel_patterns = {}
    local designs = {}
    local separator_found = false

    for line in io.lines(file_path) do
        line = line:match("^%s*(.-)%s*$") -- Trim whitespace
        if line == "" then
            separator_found = true
        elseif not separator_found then
            -- Split the first line by commas and remove whitespace
            for pattern in line:gmatch("[^,]+") do
                table.insert(towel_patterns, pattern:match("^%s*(.-)%s*$"))
            end
        else
            table.insert(designs, line)
        end
    end

    return towel_patterns, designs
end

-- Counts the number of ways the design can be formed by concatenating the towel patterns
function count_ways(design, towel_patterns)
    local dp = {}
    dp[0] = 1 -- There's one way to form an empty string

    for i = 1, #design do
        dp[i] = 0
        for _, pattern in ipairs(towel_patterns) do
            local pattern_length = #pattern
            if i >= pattern_length and design:sub(i - pattern_length + 1, i) == pattern then
                dp[i] = dp[i] + (dp[i - pattern_length] or 0)
            end
        end
    end

    return dp[#design] or 0
end

-- Counts the total number of arrangement options across all designs
function count_total_arrangements(towel_patterns, designs)
    local total = 0
    for _, design in ipairs(designs) do
        local ways = count_ways(design, towel_patterns)
        total = total + ways
    end
    return total
end


function main()

    local input_file = "input_level_19.txt"

    local towel_patterns, designs = read_input(input_file)

    local total_arrangements = count_total_arrangements(towel_patterns, designs)

    print("Total number of arrangement options: " .. total_arrangements)
end

main()
