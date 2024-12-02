-- Function to check if a report is safe based on the original rules
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

-- Function to check if a report can be made safe by removing a single level
function can_be_safe_with_dampener(report)
    for i = 1, #report do
        -- Create a new report with the ith level removed
        local modified_report = {}
        for j = 1, #report do
            if j ~= i then
                modified_report[#modified_report + 1] = report[j]
            end
        end

        if is_safe(modified_report) then
            return true
        end
    end
    return false
end

-- Function to count the number of safe reports, considering the Problem Dampener
function count_safe_reports_with_dampener(filename)
    local safe_count = 0

    for line in io.lines(filename) do
        local report = {}
        for num in line:gmatch("%S+") do
            report[#report + 1] = tonumber(num)
        end

        -- Check if the report is safe or can be made safe
        if is_safe(report) or can_be_safe_with_dampener(report) then
            safe_count = safe_count + 1
        end
    end

    return safe_count
end

-- Specify the input file
local filename = "input.txt"
local safe_reports = count_safe_reports_with_dampener(filename)
print("Number of safe reports with the Problem Dampener: " .. safe_reports)
