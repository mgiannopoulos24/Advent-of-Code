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

    -- Helper function to split strings
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

    -- Parse input file
    local function parse_input_file(input_file)
        local file = io.open(input_file, "r")
        if not file then error("Cannot open file: " .. input_file) end
        local lines = {}
        for line in file:lines() do
            table.insert(lines, line)
        end
        file:close()

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

        return registers, program
    end

    -- Run program logic
    local function execute_program(registers, program)
        local instruction_pointer = 1
        local output_values = {}

        local function get_combo_value(operand)
            if operand >= 0 and operand <= 3 then
                return operand
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

        while instruction_pointer <= #program do
            local opcode = program[instruction_pointer]
            local operand = program[instruction_pointer + 1]
            instruction_pointer = instruction_pointer + 2

            if opcode == OPCODES["adv"] then
                local denominator = 2 ^ get_combo_value(operand)
                registers["A"] = math.floor(registers["A"] / denominator)

            elseif opcode == OPCODES["bxl"] then
                registers["B"] = bitwise_xor(registers["B"], operand)

            elseif opcode == OPCODES["bst"] then
                registers["B"] = get_combo_value(operand) % 8

            elseif opcode == OPCODES["jnz"] then
                if registers["A"] ~= 0 then
                    instruction_pointer = operand + 1
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

        return output_values
    end

    -- Main logic to find the smallest A
    local function find_min_a(input_file)
        local _, program = parse_input_file(input_file)
        local program_str = table.concat(program, ",")
        local a = 0

        while true do
            local registers = { A = a, B = 0, C = 0 }
            local output = execute_program(registers, program)
            local output_str = table.concat(output, ",")
            local len_output = #output_str

            -- Compare last X digits of the program and output
            if string.sub(program_str, -len_output) == output_str then
                if output_str == program_str then
                    return a
                end
                a = a * 8  -- Multiply A by 8 as per the pattern
            else
                a = a + 1  -- Increment A when not matching fully
            end
        end
    end

    -- Run the solution
    return find_min_a
end

local input_file = "input_level_17.txt"
local find_min_a = run_program(input_file)
local result = find_min_a(input_file)
print(string.format("Lowest positive initial value for A: %.0f", result))
