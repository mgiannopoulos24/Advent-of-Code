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

-- Determines if the design can be formed by concatenating the towel patterns
function can_form_design(design, towel_patterns)
    local dp = {}
    dp[0] = true -- Empty string can always be formed

    for i = 1, #design do
        dp[i] = false
        for _, pattern in ipairs(towel_patterns) do
            local pattern_length = #pattern
            if i >= pattern_length and design:sub(i - pattern_length + 1, i) == pattern then
                if dp[i - pattern_length] then
                    dp[i] = true
                    break -- No need to check other patterns
                end
            end
        end
    end

    return dp[#design] or false
end

-- Counts how many designs can be formed using the towel patterns
function count_possible_designs(towel_patterns, designs)
    local count = 0
    for _, design in ipairs(designs) do
        if can_form_design(design, towel_patterns) then
            count = count + 1
        end
    end
    return count
end

function main()
    local input_file = "input_level_19.txt"

    local towel_patterns, designs = read_input(input_file)

    local possible_count = count_possible_designs(towel_patterns, designs)

    print("Number of possible designs: " .. possible_count)
end

main()
