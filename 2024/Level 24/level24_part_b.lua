local function read_input(file_path)
    local file = io.open(file_path, "r")
    if not file then
        error("Failed to open file: " .. file_path)
    end
    local content = file:read("*all")
    file:close()
    
    -- Normalize line endings and split into lines
    local lines = {}
    for line in content:gsub("\r\n", "\n"):gsub("\r", "\n"):gmatch("[^\n]+") do
        table.insert(lines, line)
    end
    
    -- Split into wires and gates based on content
    local wires_raw = {}
    local gates_raw = {}
    local is_gate_section = false
    
    for _, line in ipairs(lines) do
        if line:find("->") then  -- Gate line detection
            is_gate_section = true
            table.insert(gates_raw, line)
        elseif not is_gate_section then  -- Wire line
            table.insert(wires_raw, line)
        else
            error("Mixed content format")
        end
    end

    return wires_raw, gates_raw
end

local function parse_wires(wires_raw)
    local wires = {}
    for _, line in ipairs(wires_raw) do
        local name, value = line:match("([^:]+):%s*(%d+)")
        if name and value then
            wires[name] = tonumber(value)
        else
            error("Invalid wire format: " .. line)
        end
    end
    return wires
end

local function parse_gates(gates_raw)
    local gates = {}
    for _, line in ipairs(gates_raw) do
        local inputs, output = line:match("([^->]+) %-> ([^%s]+)")
        if not inputs or not output then
            error("Invalid gate format: " .. line)
        end
        local parts = {}
        for part in inputs:gmatch("%S+") do
            table.insert(parts, part)
        end
        local a, op, b
        if #parts == 3 then
            a, op, b = parts[1], parts[2], parts[3]
        elseif #parts == 2 then
            a, op = parts[1], parts[2]
            b = nil
        else
            error("Invalid gate inputs: " .. inputs)
        end
        local gate = { a = a, op = op, b = b, output = output }
        table.insert(gates, gate)
    end
    return gates
end

local function main()
    local wires_raw, gates_raw = read_input("input_level_24.txt")
    local wires = parse_wires(wires_raw)
    local input_bit_count = math.floor(#wires_raw / 2)
    local gates = parse_gates(gates_raw)

    local flags = {}

    -- Categorize gates
    local FA_GATE0 = {}
    local FA_GATE3 = {}
    for _, gate in ipairs(gates) do
        if gate.op == "XOR" then
            local a_start = gate.a:sub(1, 1) == 'x'
            local b_start = gate.b and gate.b:sub(1, 1) == 'x'
            if a_start or b_start then
                table.insert(FA_GATE0, gate)
            else
                table.insert(FA_GATE3, gate)
            end
        end
    end

    local output_gates = {}
    for _, gate in ipairs(gates) do
        if gate.output:sub(1, 1) == 'z' then
            table.insert(output_gates, gate)
        end
    end

    -- Check FA_GATE0 gates
    for _, gate in ipairs(FA_GATE0) do
        local a = gate.a
        local b = gate.b
        local output = gate.output
        local is_first = (a == "x00") or (b == "x00")
        if is_first then
            if output ~= "z00" then
                flags[output] = true
            end
        else
            if output == "z00" then
                flags[output] = true
            elseif output:sub(1, 1) == 'z' then
                flags[output] = true
            end
        end
    end

    -- Check FA_GATE3 gates
    for _, gate in ipairs(FA_GATE3) do
        if gate.output:sub(1, 1) ~= 'z' then
            flags[gate.output] = true
        end
    end

    -- Check output gates
    for _, gate in ipairs(output_gates) do
        local output = gate.output
        local is_last = output == string.format("z%02d", input_bit_count)
        if is_last then
            if gate.op ~= "OR" then
                flags[output] = true
            end
        else
            if gate.op ~= "XOR" then
                flags[output] = true
            end
        end
    end

    -- Additional checks for FA_GATE0
    local check_next = {}
    for _, gate in ipairs(FA_GATE0) do
        local output = gate.output
        if not flags[output] and output ~= "z00" then
            local matches = {}
            for _, g3 in ipairs(FA_GATE3) do
                if g3.a == output or g3.b == output then
                    table.insert(matches, g3)
                end
            end
            if #matches == 0 then
                table.insert(check_next, gate)
                flags[output] = true
            end
        end
    end

    for _, gate in ipairs(check_next) do
        local a = gate.a
        local output = gate.output
        local num = a:sub(2)
        local intended_result = "z" .. num
        local matches = {}
        for _, g3 in ipairs(FA_GATE3) do
            if g3.output == intended_result then
                table.insert(matches, g3)
            end
        end
        if #matches ~= 1 then
            error("Wrong input.")
        end
        local match = matches[1]
        local to_check = { match.a, match.b }
        local or_matches = {}
        for _, g in ipairs(gates) do
            if g.op == "OR" and (g.output == to_check[1] or g.output == to_check[2]) then
                table.insert(or_matches, g)
            end
        end
        if #or_matches ~= 1 then
            error("Too complex.")
        end
        local or_output = or_matches[1].output
        local correct_output
        for _, wire in ipairs(to_check) do
            if wire ~= or_output then
                correct_output = wire
                break
            end
        end
        flags[correct_output] = true
    end

    -- Validate and print
    local count = 0
    for _ in pairs(flags) do
        count = count + 1
    end
    if count ~= 8 then
        error("Too complex.")
    end

    local sorted_flags = {}
    for k in pairs(flags) do
        table.insert(sorted_flags, k)
    end
    table.sort(sorted_flags)
    print(table.concat(sorted_flags, ","))
end

main()
