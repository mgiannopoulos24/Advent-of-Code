local function parse_schematics(input_file)
    local file = io.open(input_file, 'r')
    local content = file:read('*all')
    file:close()

    -- Split content into lines, preserving empty lines
    local lines = {}
    local pos = 1
    while pos <= #content do
        local nl = content:find('\n', pos)
        if nl then
            table.insert(lines, content:sub(pos, nl-1))
            pos = nl + 1
        else
            table.insert(lines, content:sub(pos))
            break
        end
    end

    -- Group lines into schematics separated by empty lines
    local schematics = {}
    local current_block = {}
    for _, line in ipairs(lines) do
        if line == "" then
            if #current_block > 0 then
                table.insert(schematics, current_block)
                current_block = {}
            end
        else
            table.insert(current_block, line)
        end
    end
    if #current_block > 0 then
        table.insert(schematics, current_block)
    end

    local locks = {}
    local keys = {}

    for _, schematic in ipairs(schematics) do
        local top_row = schematic[1]
        local bottom_row = schematic[#schematic]

        -- Check if lock
        local is_lock = not top_row:find("[^#]") and not bottom_row:find("[^.]")
        if is_lock then
            table.insert(locks, schematic)
        else
            -- Check if key
            local is_key = not top_row:find("[^.]") and not bottom_row:find("[^#]")
            if is_key then
                table.insert(keys, schematic)
            end
        end
    end

    return locks, keys
end

local function convert_to_heights(schematic, is_lock)
    local height_list = {}
    if #schematic == 0 then return height_list end
    local num_cols = #schematic[1]
    for col = 1, num_cols do
        local column = {}
        for _, row in ipairs(schematic) do
            table.insert(column, row:sub(col, col))
        end
        if is_lock then
            local count = 0
            for _, c in ipairs(column) do
                if c == '#' then count = count + 1 end
            end
            table.insert(height_list, count)
        else
            local reversed_col = {}
            for i = #column, 1, -1 do
                table.insert(reversed_col, column[i])
            end
            local count = 0
            for _, c in ipairs(reversed_col) do
                if c == '#' then count = count + 1 end
            end
            table.insert(height_list, count)
        end
    end
    return height_list
end

local function count_fitting_pairs(locks, keys)
    local lock_heights = {}
    for _, lock in ipairs(locks) do
        table.insert(lock_heights, convert_to_heights(lock, true))
    end

    local key_heights = {}
    for _, key in ipairs(keys) do
        table.insert(key_heights, convert_to_heights(key, false))
    end

    local total_pairs = 0
    if #locks == 0 or #keys == 0 then return total_pairs end
    local row_count = #locks[1]

    for _, lock in ipairs(lock_heights) do
        for _, key in ipairs(key_heights) do
            local fits = true
            for col = 1, #lock do
                if lock[col] + key[col] > row_count then
                    fits = false
                    break
                end
            end
            if fits then
                total_pairs = total_pairs + 1
            end
        end
    end

    return total_pairs
end

local function main()
    local input_file = "input_level_25.txt"
    local locks, keys = parse_schematics(input_file)
    local result = count_fitting_pairs(locks, keys)
    print("Number of unique lock/key pairs that fit: " .. result)
end

main()
