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

-- Update robot positions based on velocities and wrapping
function update_positions(robots, width, height)
    for _, robot in ipairs(robots) do
        local pos = robot[1]
        local vel = robot[2]
        
        -- Update positions with wrapping
        pos[1] = (pos[1] + vel[1] + width) % width
        pos[2] = (pos[2] + vel[2] + height) % height
    end
end

-- Create a grid and populate it with robot positions
function create_grid(robots, width, height)
    local grid = {}
    for y = 1, height do
        grid[y] = {}
        for x = 1, width do
            grid[y][x] = "."
        end
    end

    for _, robot in ipairs(robots) do
        local x, y = robot[1][1] + 1, robot[1][2] + 1  -- Adjust for 1-based indexing
        grid[y][x] = "#"
    end

    return grid
end

-- Calculate togetherness by checking neighbors
function calc_togetherness(grid, width, height)
    local togetherness = 0
    local directions = {{0, -1}, {0, 1}, {-1, 0}, {1, 0}} -- Up, Down, Left, Right

    for y = 1, height do
        for x = 1, width do
            if grid[y][x] == "#" then
                for _, dir in ipairs(directions) do
                    local nx, ny = x + dir[1], y + dir[2]
                    if nx > 0 and nx <= width and ny > 0 and ny <= height and grid[ny][nx] == "#" then
                        togetherness = togetherness + 1
                        break
                    end
                end
            end
        end
    end

    return togetherness
end

function main()

    local filename = "input_level_14.txt"
    
    -- Space dimensions
    local width = 101
    local height = 103
    
    -- Maximum time to simulate
    local max_seconds = 10000

    local robots = parse_input(filename)

    local max_togetherness = 0
    local best_time = 0

    for time = 0, max_seconds do

        update_positions(robots, width, height)

        local grid = create_grid(robots, width, height)

        local togetherness = calc_togetherness(grid, width, height)

        -- Check if this is the maximum togetherness
        if togetherness > max_togetherness then
            max_togetherness = togetherness
            best_time = time + 1 

            print("Time:", time + 1)  -- Display 1-based time
            for _, row in ipairs(grid) do
                print(table.concat(row))
            end
        end
    end

    print("Maximum Togetherness:", max_togetherness, "at time:", best_time)
end

main()
