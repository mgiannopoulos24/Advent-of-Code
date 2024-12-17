function run_program(input_file)
    -- Define constants for opcodes
    local OPCODES = {
        adv = 0,  -- Divide A and write result to A
        bxl = 1,  -- B = B XOR literal
        bst = 2,  -- B = combo_operand % 8
        jnz = 3,  -- Jump if A != 0
        bxc = 4,  -- B = B XOR C (operand ignored)
        out = 5,  -- Output combo_operand % 8
        bdv = 6,  -- Divide A and write result to B
        cdv = 7   -- Divide A and write result to C
    }

    -- Helper function to split strings (since Lua lacks split)
    local function split(inputstr, sep)
        if sep == nil then sep = "%s" end
        local t = {}
        for str in string.gmatch(inputstr, "([^"..sep.."]+)") do
            table.insert(t, str)
        end
        return t
    end

    -- Manual bitwise XOR implementation
    local function bitwise_xor(a, b)
        local result = 0
        local bit = 1
        while a > 0 or b > 0 do
            local a_bit = a % 2
            local b_bit = b % 2
            if a_bit ~= b_bit then
                result = result + bit
            end
            a = math.floor(a / 2)
            b = math.floor(b / 2)
            bit = bit * 2
        end
        return result
    end

    -- Read input file and parse lines
    local file = io.open(input_file, "r")
    if not file then error("Cannot open file: " .. input_file) end
    local lines = {}
    for line in file:lines() do
        table.insert(lines, line)
    end
    file:close()

    -- Initialize registers and program
    local registers = { A = 0, B = 0, C = 0 }
    local program = {}

    for _, line in ipairs(lines) do
        if string.sub(line, 1, 8) == "Register" then
            local reg, value = line:match("Register (%w): (%d+)")
            registers[reg] = tonumber(value)
        elseif string.sub(line, 1, 7) == "Program" then
            local program_values = split(line:match("Program: (.+)"), ",")
            for _, v in ipairs(program_values) do
                table.insert(program, tonumber(v))
            end
        end
    end

    -- Helper function to get combo operand value
    local function get_combo_value(operand)
        if operand >= 0 and operand <= 3 then
            return operand  -- Literal values
        elseif operand == 4 then
            return registers["A"]
        elseif operand == 5 then
            return registers["B"]
        elseif operand == 6 then
            return registers["C"]
        else
            error("Invalid combo operand: " .. operand)
        end
    end

    -- Initialize variables
    local instruction_pointer = 1  -- Lua indices start at 1
    local output_values = {}

    -- Run program
    while instruction_pointer <= #program do
        local opcode = program[instruction_pointer]
        local operand = program[instruction_pointer + 1]
        instruction_pointer = instruction_pointer + 2  -- Default advance unless JNZ alters it

        if opcode == OPCODES["adv"] then
            local denominator = 2 ^ get_combo_value(operand)
            registers["A"] = math.floor(registers["A"] / denominator)

        elseif opcode == OPCODES["bxl"] then
            registers["B"] = bitwise_xor(registers["B"], operand)

        elseif opcode == OPCODES["bst"] then
            registers["B"] = get_combo_value(operand) % 8

        elseif opcode == OPCODES["jnz"] then
            if registers["A"] ~= 0 then
                instruction_pointer = operand + 1  -- Adjust for Lua's 1-based indexing
            end

        elseif opcode == OPCODES["bxc"] then
            registers["B"] = bitwise_xor(registers["B"], registers["C"])

        elseif opcode == OPCODES["out"] then
            table.insert(output_values, get_combo_value(operand) % 8)

        elseif opcode == OPCODES["bdv"] then
            local denominator = 2 ^ get_combo_value(operand)
            registers["B"] = math.floor(registers["A"] / denominator)

        elseif opcode == OPCODES["cdv"] then
            local denominator = 2 ^ get_combo_value(operand)
            registers["C"] = math.floor(registers["A"] / denominator)

        else
            error("Invalid opcode: " .. opcode)
        end
    end

    -- Join output values with commas and return
    return table.concat(output_values, ",")
end

local input_file = "input_level_17.txt"
local output = run_program(input_file)
print("Final Output: " .. output)
