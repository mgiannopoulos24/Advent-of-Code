local keypad_layout = {"789", "456", "123", " 0A"}
local keypad = {}
for y, row in ipairs(keypad_layout) do
    for x = 1, #row do
        local char = row:sub(x, x)
        if char ~= ' ' then
            keypad[char] = {x = x - 1, y = y - 1}
        end
    end
end

local direction_layout = {" ^A", "<v>"}
local directions = {}
for y, row in ipairs(direction_layout) do
    for x = 1, #row do
        local char = row:sub(x, x)
        if char ~= ' ' then
            directions[char] = {x = x - 1, y = y - 1}
        end
    end
end

local move_map = {
    ['^'] = {dx = 0, dy = -1},
    ['>'] = {dx = 1, dy = 0},
    ['v'] = {dx = 0, dy = 1},
    ['<'] = {dx = -1, dy = 0}
}

local cache = {}

local function generate_unique_perms(arr)
    local perms = {}
    local seen = {}
    local function helper(start)
        if start == #arr then
            local key = table.concat(arr)
            if not seen[key] then
                seen[key] = true
                table.insert(perms, {table.unpack(arr)})
            end
            return
        end
        for i = start, #arr do
            arr[start], arr[i] = arr[i], arr[start]
            helper(start + 1)
            arr[start], arr[i] = arr[i], arr[start]
        end
    end
    helper(1)
    return perms
end

local function calculate_button_presses(sequence, max_depth, use_direction, current_pos)
    max_depth = max_depth or 25
    use_direction = use_direction or false

    local active_keypad = use_direction and directions or keypad

    local current_pos_coords
    if not current_pos then
        current_pos_coords = active_keypad['A']
    else
        current_pos_coords = current_pos
    end

    local current_pos_str = current_pos_coords.x .. "," .. current_pos_coords.y
    local key = table.concat({
        sequence,
        tostring(max_depth),
        tostring(use_direction),
        current_pos_str
    }, "|")

    if cache[key] then
        return cache[key]
    end

    if #sequence == 0 then
        cache[key] = 0
        return 0
    end

    local target_char = sequence:sub(1, 1)
    local remaining_sequence = sequence:sub(2)

    local target = active_keypad[target_char]
    if not target then
        error("Invalid character: " .. target_char)
    end

    local current_x, current_y = current_pos_coords.x, current_pos_coords.y
    local target_x, target_y = target.x, target.y

    local delta_x = target_x - current_x
    local delta_y = target_y - current_y

    local move_sequence = ""
    if delta_x > 0 then
        move_sequence = string.rep(">", delta_x)
    elseif delta_x < 0 then
        move_sequence = string.rep("<", -delta_x)
    end

    if delta_y > 0 then
        move_sequence = move_sequence .. string.rep("v", delta_y)
    elseif delta_y < 0 then
        move_sequence = move_sequence .. string.rep("^", -delta_y)
    end

    local min_perm_length
    if max_depth > 0 then
        local moves = {}
        for c in move_sequence:gmatch(".") do
            table.insert(moves, c)
        end

        local unique_perms = #moves > 0 and generate_unique_perms(moves) or {}

        local perm_lengths = {}
        for _, perm in ipairs(unique_perms) do
            local temp_x = current_x
            local temp_y = current_y
            local valid = true
            for _, move in ipairs(perm) do
                local delta = move_map[move]
                temp_x = temp_x + delta.dx
                temp_y = temp_y + delta.dy

                local pos_valid = false
                for _, pos in pairs(active_keypad) do
                    if pos.x == temp_x and pos.y == temp_y then
                        pos_valid = true
                        break
                    end
                end
                if not pos_valid then
                    valid = false
                    break
                end
            end

            if valid then
                local new_sequence = table.concat(perm) .. 'A'
                local sub_presses = calculate_button_presses(new_sequence, max_depth - 1, true)
                table.insert(perm_lengths, sub_presses)
            end
        end

        if #perm_lengths > 0 then
            min_perm_length = math.min(table.unpack(perm_lengths))
        else
            min_perm_length = #move_sequence + 1
        end
    else
        min_perm_length = #move_sequence + 1
    end

    local remaining_presses = calculate_button_presses(remaining_sequence, max_depth, use_direction, target)
    local total_presses = min_perm_length + remaining_presses

    cache[key] = total_presses
    return total_presses
end

local file = io.open("input_level_21.txt", "r")
local input_codes = {}
for line in file:lines() do
    table.insert(input_codes, line)
end
file:close()

local total_presses = 0
for _, code in ipairs(input_codes) do
    local code_str = code
    local code_value = tonumber(code_str:sub(1, -2)) or 0
    local presses = calculate_button_presses(code_str)
    total_presses = total_presses + code_value * presses
end

print(total_presses)
