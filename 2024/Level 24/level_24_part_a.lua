local function parse_input(filename)
    -- Parses the input file into initial values and gate definitions.
    local initial_values = {}
    local gates = {}

    for line in io.lines(filename) do
        line = line:match("^%s*(.-)%s*$") -- Trim whitespace
        if line ~= "" then
            if line:find(":") then
                -- Initial values
                local wire, value = line:match("(.-):%s*(%d+)")
                initial_values[wire] = tonumber(value)
            else
                -- Gate definitions
                table.insert(gates, line)
            end
        end
    end

    return initial_values, gates
end

local function evaluate_gate(input1, input2, operation)
    -- Evaluates the result of a boolean gate.
    if operation == "AND" then
        return input1 & input2
    elseif operation == "OR" then
        return input1 | input2
    elseif operation == "XOR" then
        return input1 ~ input2
    else
        error("Unknown operation: " .. operation)
    end
end

local function simulate_system(initial_values, gates)
    -- Simulates the boolean logic gates system.
    local wire_values = {}
    for k, v in pairs(initial_values) do
        wire_values[k] = v
    end

    while #gates > 0 do
        local remaining_gates = {}

        for _, gate in ipairs(gates) do
            local parts = {}
            for word in gate:gmatch("%S+") do
                table.insert(parts, word)
            end

            if #parts == 5 then
                -- Format: input1 OPERATION input2 -> output
                local input1, operation, input2, _, output = parts[1], parts[2], parts[3], parts[4], parts[5]

                -- Check if inputs are ready
                if wire_values[input1] ~= nil and wire_values[input2] ~= nil then
                    wire_values[output] = evaluate_gate(wire_values[input1], wire_values[input2], operation)
                else
                    table.insert(remaining_gates, gate)
                end
            end
        end

        if #remaining_gates == #gates then
            error("Stuck in a loop! Some gates cannot be resolved.")
        end

        gates = remaining_gates
    end

    return wire_values
end

local function compute_output(wire_values)
    -- Computes the final decimal output based on wires starting with 'z'.
    local binary_output = ""

    local z_wires = {}
    for wire, _ in pairs(wire_values) do
        if wire:sub(1, 1) == "z" then
            table.insert(z_wires, wire)
        end
    end

    table.sort(z_wires)

    for _, wire in ipairs(z_wires) do
        binary_output = wire_values[wire] .. binary_output
    end

    return tonumber(binary_output, 2)
end

local function main()
    local filename = "input_level_24.txt" 

    local initial_values, gates = parse_input(filename)

    local wire_values = simulate_system(initial_values, gates)

    local output = compute_output(wire_values)
    print("The final output is: " .. output)
end

main()
