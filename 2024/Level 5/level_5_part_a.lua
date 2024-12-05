-- Helper function to trim whitespace from both ends of a string
local function trim(s)
    return (s:gsub("^%s*(.-)%s*$", "%1"))
end

-- Helper function to split a string by a given delimiter
local function split(inputstr, sep)
    sep = sep or "%s"
    local t = {}
    for str in string.gmatch(inputstr, "([^" .. sep .. "]+)") do
        table.insert(t, trim(str))
    end
    return t
end

-- Function to read and parse the input file
local function read_input(file_path)
    local file = io.open(file_path, "r")
    if not file then
        error("Cannot open file: " .. file_path)
    end

    local content = file:read("*all")
    file:close()

    -- Split the content into rules and updates based on the first blank line
    local rules_section, updates_section = content:match("^(.-)\r?\n\r?\n(.*)$")
    if not rules_section or not updates_section then
        error("Input file must contain two sections separated by at least one blank line.")
    end

    -- Parse rules from the first section
    local rules = {}
    for line in string.gmatch(rules_section, "[^\r\n]+") do
        local parts = split(line, "|")
        if #parts ~= 2 then
            error("Invalid rule format (expected 'X|Y'): " .. line)
        end
        local x = tonumber(parts[1])
        local y = tonumber(parts[2])
        if not x or not y then
            error("Non-integer values in rule: " .. line)
        end
        table.insert(rules, {x, y})
    end

    -- Parse updates from the second section
    local updates = {}
    for line in string.gmatch(updates_section, "[^\r\n]+") do
        local num_strs = split(line, ",")
        local update = {}
        for _, num_str in ipairs(num_strs) do
            local num = tonumber(num_str)
            if not num then
                error("Non-integer value in update: " .. num_str)
            end
            table.insert(update, num)
        end
        table.insert(updates, update)
    end

    return rules, updates
end

-- Function to check if an update is ordered according to the rules
local function is_update_ordered(update, rules)
    local page_positions = {}
    for idx, page in ipairs(update) do
        page_positions[page] = idx
    end

    for _, rule in ipairs(rules) do
        local x, y = rule[1], rule[2]
        if page_positions[x] and page_positions[y] then
            if page_positions[x] > page_positions[y] then
                return false
            end
        end
    end

    return true
end

-- Function to find the middle page of an update
local function find_middle_page(update)
    local len = #update
    if len == 0 then
        return nil
    end
    local middle_index = math.floor((len + 1) / 2)  
    return update[middle_index]
end

-- Main solve function
local function solve(file_path)
    local rules, updates = read_input(file_path)
    local middle_pages_sum = 0

    for _, update in ipairs(updates) do
        if is_update_ordered(update, rules) then
            local middle_page = find_middle_page(update)
            if middle_page then
                middle_pages_sum = middle_pages_sum + middle_page
            end
        end
    end

    return middle_pages_sum
end

-- Entry point
local function main()
    local input_file = "input_level_5.txt"  
    local status, result = pcall(solve, input_file)
    if status then
        print(string.format("Sum of middle page numbers from correctly ordered updates: %d", result))
    else
        print("Error:", result)
    end
end

main()