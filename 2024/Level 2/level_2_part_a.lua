-- Function to check if a report is safe based on given rules
function is_safe(report)
    local differences = {}
    for i = 1, #report - 1 do
        differences[#differences + 1] = report[i + 1] - report[i]
    end

    local all_increasing = true
    local all_decreasing = true

    -- Check if all differences are within the range for increasing or decreasing
    for _, diff in ipairs(differences) do
        if diff < 1 or diff > 3 then
            all_increasing = false
        end
        if diff > -1 or diff < -3 then
            all_decreasing = false
        end
    end

    return all_increasing or all_decreasing
end

-- Function to count the number of safe reports in the input file
function count_safe_reports(filename)
    local safe_count = 0

    for line in io.lines(filename) do
        local report = {}
        for num in line:gmatch("%S+") do
            report[#report + 1] = tonumber(num)
        end

        if is_safe(report) then
            safe_count = safe_count + 1
        end
    end

    return safe_count
end

-- Specify the input file
local filename = "input_level_2.txt"
local safe_reports = count_safe_reports(filename)
print("Number of safe reports: " .. safe_reports)
