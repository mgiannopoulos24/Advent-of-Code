local keypad_layout = {'789', '456', '123', ' 0A'}
local keypad = {}
for y, row in ipairs(keypad_layout) do
    for x = 1, #row do
        local char = row:sub(x, x)
        if char ~= ' ' then
            keypad[char] = {x - 1, y - 1}
        end
    end
end

local direction_layout = {' ^A', '<v>'}
local directions = {}
for y, row in ipairs(direction_layout) do
    for x = 1, #row do
        local char = row:sub(x, x)
        if char ~= ' ' then
            directions[char] = {x - 1, y - 1}
        end
    end
end

local move_map = {
    ['^'] = {0, -1},
    ['>'] = {1, 0},
    ['v'] = {0, 1},
    ['<'] = {-1, 0}
}

local memo = {}

local function coordinate_in_active_keypad(x, y, active_keypad)
    for _, coord in pairs(active_keypad) do
        if coord[1] == x and coord[2] == y then
            return true
        end
    end
    return false
end

local function permutations(str)
    local seen = {}
    local result = {}
    local function recurse(s, prefix)
        if #s == 0 then
            if not seen[prefix] then
                table.insert(result, prefix)
                seen[prefix] = true
            end
            return
        end
        for i = 1, #s do
            local char = s:sub(i, i)
            local rest = s:sub(1, i - 1) .. s:sub(i + 1)
            recurse(rest, prefix .. char)
        end
    end
    recurse(str, "")
    return result
end

local function calculate_button_presses(sequence, max_depth, use_direction, current_pos)
    max_depth = max_depth or 2
    use_direction = use_direction or false
    current_pos = current_pos or nil

    local current_pos_str = "nil"
    if current_pos then
        current_pos_str = table.concat(current_pos, ",")
    end
    local memo_key = table.concat({sequence, tostring(max_depth), tostring(use_direction), current_pos_str}, "|")

    if memo[memo_key] ~= nil then
        return memo[memo_key]
    end

    if #sequence == 0 then
        memo[memo_key] = 0
        return 0
    end

    local active_keypad = use_direction and directions or keypad

    if not current_pos then
        current_pos = active_keypad['A']
    end

    local current_x, current_y = current_pos[1], current_pos[2]
    local first_char = sequence:sub(1, 1)
    local target = active_keypad[first_char]
    if not target then
        error("Character not found in keypad: " .. first_char)
    end
    local target_x, target_y = target[1], target[2]
    local delta_x = target_x - current_x
    local delta_y = target_y - current_y

    local move_sequence = ""
    if delta_x > 0 then
        move_sequence = move_sequence .. string.rep(">", delta_x)
    elseif delta_x < 0 then
        move_sequence = move_sequence .. string.rep("<", -delta_x)
    end
    if delta_y > 0 then
        move_sequence = move_sequence .. string.rep("v", delta_y)
    elseif delta_y < 0 then
        move_sequence = move_sequence .. string.rep("^", -delta_y)
    end

    local min_perm_length
    if max_depth > 0 then
        local perm_lengths = {}
        local perms = permutations(move_sequence)
        for _, perm in ipairs(perms) do
            local temp_x, temp_y = current_x, current_y
            local valid = true
            for i = 1, #perm do
                local move = perm:sub(i, i)
                local delta = move_map[move]
                temp_x = temp_x + delta[1]
                temp_y = temp_y + delta[2]
                if not coordinate_in_active_keypad(temp_x, temp_y, active_keypad) then
                    valid = false
                    break
                end
            end
            if valid then
                local new_sequence = perm .. "A"
                local presses = calculate_button_presses(new_sequence, max_depth - 1, true)
                table.insert(perm_lengths, presses)
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

    local rest_sequence = #sequence > 1 and sequence:sub(2) or ""
    local rest_presses = calculate_button_presses(rest_sequence, max_depth, use_direction, {target_x, target_y})

    local total_presses = min_perm_length + rest_presses
    memo[memo_key] = total_presses
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
    local code_value = tonumber(code:sub(1, -2))
    local presses = calculate_button_presses(code)
    total_presses = total_presses + code_value * presses
end

print(total_presses)
