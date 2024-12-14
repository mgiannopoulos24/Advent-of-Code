-- Parse input file to extract positions and velocities
function parse_input(filename)
    local robots = {}
    local file = io.open(filename, "r")
    for line in file:lines() do
        local parts = {}
        for part in string.gmatch(line, "%S+") do
            table.insert(parts, part)
        end

        local pos = {}
        for val in string.gmatch(parts[1]:sub(2), "(-?%d+)") do
            table.insert(pos, tonumber(val))
        end

        local vel = {}
        for val in string.gmatch(parts[2]:sub(2), "(-?%d+)") do
            table.insert(vel, tonumber(val))
        end

        table.insert(robots, {pos, vel})
    end
    file:close()
    return robots
end

-- Update robot positions based on velocities and time, wrapping around the edges
function update_positions(robots, width, height, time)
    local updated_positions = {}
    for _, robot in ipairs(robots) do
        local pos = robot[1]
        local vel = robot[2]
        
        -- Calculate new positions with wrapping
        local new_x = (pos[1] + vel[1] * time) % width
        local new_y = (pos[2] + vel[2] * time) % height
        table.insert(updated_positions, {new_x, new_y})
    end
    return updated_positions
end

-- Count robots in each of the four quadrants
function count_robots_in_quadrants(positions, width, height)
    local mid_x = math.floor(width / 2)
    local mid_y = math.floor(height / 2)

    local quadrants = {0, 0, 0, 0}  -- Top-left, Top-right, Bottom-left, Bottom-right

    for _, pos in ipairs(positions) do
        local x, y = pos[1], pos[2]
        if x == mid_x or y == mid_y then
            -- Ignore robots exactly on the middle lines
            goto continue
        end

        if x < mid_x and y < mid_y then
            quadrants[1] = quadrants[1] + 1
        elseif x >= mid_x and y < mid_y then
            quadrants[2] = quadrants[2] + 1
        elseif x < mid_x and y >= mid_y then
            quadrants[3] = quadrants[3] + 1
        else
            quadrants[4] = quadrants[4] + 1
        end

        ::continue::
    end
    return quadrants
end

-- Calculate the safety factor by multiplying the counts of robots in each quadrant
function calculate_safety_factor(quadrants)
    local factor = 1
    for _, count in ipairs(quadrants) do
        factor = factor * count
    end
    return factor
end

function main()

    local filename = "input_level_14.txt"
    
    -- Space dimensions
    local width = 101
    local height = 103
    
    -- Time to simulate
    local time = 100

    local robots = parse_input(filename)

    -- Update positions after 100 seconds
    local updated_positions = update_positions(robots, width, height, time)

    local quadrants = count_robots_in_quadrants(updated_positions, width, height)

    local safety_factor = calculate_safety_factor(quadrants)

    print("Quadrants: " .. table.concat(quadrants, ", "))
    print("Safety Factor: " .. safety_factor)
end

-- Run the main function
main()
