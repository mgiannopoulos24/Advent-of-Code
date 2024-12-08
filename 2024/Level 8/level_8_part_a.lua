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

-- Calculates antinodes created by antenna pairs
local function calculate_antinodes(antennas, map_width, map_height)
    local antinodes = {}

    local function add_antinode(x, y)
        if x >= 0 and x < map_height and y >= 0 and y < map_width then
            antinodes[x .. "," .. y] = true -- Use a table as a set
        end
    end

    for i = 1, #antennas do
        for j = 1, #antennas do
            if i ~= j then
                local x1, y1, freq1 = table.unpack(antennas[i])
                local x2, y2, freq2 = table.unpack(antennas[j])

                if freq1 == freq2 then
                    local dx, dy = x2 - x1, y2 - y1

                    -- Calculate the two antinode positions
                    local antinode1_x, antinode1_y = x1 - dx, y1 - dy
                    local antinode2_x, antinode2_y = x2 + dx, y2 + dy

                    -- Add antinodes within bounds
                    add_antinode(antinode1_x, antinode1_y)
                    add_antinode(antinode2_x, antinode2_y)
                end
            end
        end
    end

    return antinodes
end

-- Counts the unique antinode locations within the map bounds
local function count_unique_antinode_locations(file_path)
    local map_data = read_map(file_path)
    local map_height, map_width = #map_data, #map_data[1]
    local antennas = find_antennas(map_data)
    local antinodes = calculate_antinodes(antennas, map_width, map_height)

    -- Count unique antinodes
    local count = 0
    for _ in pairs(antinodes) do
        count = count + 1
    end
    return count
end

-- Example usage
local input_file = "input_level_8.txt" -- Replace with your file path
local result = count_unique_antinode_locations(input_file)
print("Unique antinode locations:", result)
