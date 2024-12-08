local function gcd(a, b)
    while b ~= 0 do
        a, b = b, a % b
    end
    return math.abs(a)
end

-- Reads the map from a file
local function read_map(file_path)
    local map = {}
    for line in io.lines(file_path) do
        local row = {}
        for char in line:gmatch(".") do
            table.insert(row, char)
        end
        table.insert(map, row)
    end
    return map
end

-- Identifies all antennas and their frequencies on the map
local function find_antennas(map_data)
    local antennas = {}
    for row_idx, row in ipairs(map_data) do
        for col_idx, char in ipairs(row) do
            if char:match("%w") then -- Alphanumeric characters are antennas
                table.insert(antennas, {row_idx - 1, col_idx - 1, char}) -- Convert to 0-based indexing
            end
        end
    end
    return antennas
end

-- Calculates all antinode locations
local function calculate_antinodes_part_two(antennas, map_width, map_height)
    local antinodes = {}
    local freq_groups = {}

    -- Group antennas by frequency
    for _, antenna in ipairs(antennas) do
        local x, y, freq = table.unpack(antenna)
        freq_groups[freq] = freq_groups[freq] or {}
        table.insert(freq_groups[freq], {x, y})
    end

    -- Add antinodes based on frequency groups
    for _, freq_antennas in pairs(freq_groups) do
        -- Add antennas themselves as antinodes if they share a frequency
        if #freq_antennas > 1 then
            for _, pos in ipairs(freq_antennas) do
                local key = pos[1] .. "," .. pos[2]
                antinodes[key] = true
            end
        end

        -- Check all pairs of antennas for this frequency
        for i = 1, #freq_antennas do
            for j = i + 1, #freq_antennas do
                local x1, y1 = table.unpack(freq_antennas[i])
                local x2, y2 = table.unpack(freq_antennas[j])

                -- Calculate steps for alignment
                local dx, dy = x2 - x1, y2 - y1
                local step_gcd = gcd(dx, dy)
                local step_x, step_y = dx // step_gcd, dy // step_gcd

                -- Generate all positions between and beyond the antennas
                local k = 1
                while true do -- Forward direction
                    local nx, ny = x2 + k * step_x, y2 + k * step_y
                    if nx >= 0 and nx < map_height and ny >= 0 and ny < map_width then
                        antinodes[nx .. "," .. ny] = true
                    else
                        break
                    end
                    k = k + 1
                end

                k = 1
                while true do -- Backward direction
                    local nx, ny = x1 - k * step_x, y1 - k * step_y
                    if nx >= 0 and nx < map_height and ny >= 0 and ny < map_width then
                        antinodes[nx .. "," .. ny] = true
                    else
                        break
                    end
                    k = k + 1
                end
            end
        end
    end

    return antinodes
end

-- Counts the unique antinode locations for Part Two
local function count_unique_antinode_locations_part_two(file_path)
    local map_data = read_map(file_path)
    local map_height, map_width = #map_data, #map_data[1]
    local antennas = find_antennas(map_data)
    local antinodes = calculate_antinodes_part_two(antennas, map_width, map_height)

    -- Count unique antinodes
    local count = 0
    for _ in pairs(antinodes) do
        count = count + 1
    end
    return count
end

-- Example usage
local input_file = "input_level_8.txt" -- Replace with your file path
local result = count_unique_antinode_locations_part_two(input_file)
print("Unique antinode locations:", result)
