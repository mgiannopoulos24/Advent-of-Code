-- Parse the input file and initialize the grid and movements
function parse_input(filename)
    local file = io.open(filename, "r")
    local content = file:read("*all")
    file:close()

    local grid_raw, movements = content:match("^(.-)%s%s(.*)$")
    movements = movements:gsub("\n", "")

    -- Split the grid into a 2D array
    local grid = {}
    for row in grid_raw:gmatch("[^\n]+") do
        local row_table = {}
        for char in row:gmatch(".") do
            table.insert(row_table, char)
        end
        table.insert(grid, row_table)
    end
    return grid, movements
end

-- Create a bound table with positional and size information
function create_bound(x, y, width, height)
    return { x = x, y = y, width = width or 0, height = height or 0 }
end

-- Check if two bounds collide
function does_collide(bound1, bound2)
    return bound1.x < bound2.x + bound2.width and
           bound1.x + bound1.width > bound2.x and
           bound1.y < bound2.y + bound2.height and
           bound1.y + bound1.height > bound2.y
end

-- Find the starting position of the player (@)
function find_starting_position(grid)
    for i = 1, #grid do
        for j = 1, #grid[i] do
            if grid[i][j] == "@" then
                return {
                    x = (j - 1) * 2 + 1,
                    y = i,
                    bound = create_bound((j - 1), i - 1, 1, 1)
                }
            end
        end
    end
    return nil -- Return nil if not found
end

-- Initialize boxes and walls from the grid
function initialize_objects(grid)
    local boxes = {}
    local walls = {}

    for y = 1, #grid do
        for x = 1, #grid[y] do
            local cell = grid[y][x]
            local pos_x = (x - 1) * 2 + 1
            if cell == "O" then
                table.insert(boxes, {
                    x = pos_x,
                    y = y,
                    bound = create_bound((x - 1) * 2, y - 1, 2, 1),
                    id = #boxes + 1
                })
            elseif cell == "#" then
                table.insert(walls, {
                    x = pos_x,
                    y = y,
                    bound = create_bound((x - 1) * 2, y - 1, 2, 1)
                })
            end
        end
    end
    return boxes, walls
end

-- Process movements and update player and boxes positions
function process_movements(movements, player_pos, boxes, walls)
    local directions = {
        ["^"] = { x = 0, y = -1 },
        ["v"] = { x = 0, y = 1 },
        ["<"] = { x = -1, y = 0 },
        [">"] = { x = 1, y = 0 },
    }

    for mov in movements:gmatch(".") do
        local dir = directions[mov]
        local new_player_pos = {
            x = player_pos.x + dir.x,
            y = player_pos.y + dir.y,
            bound = create_bound(player_pos.x + dir.x - 1, player_pos.y + dir.y - 1, 1, 1)
        }

        -- Check collision with walls
        local collision_with_wall = false
        for _, wall in ipairs(walls) do
            if does_collide(new_player_pos.bound, wall.bound) then
                collision_with_wall = true
                break
            end
        end
        if collision_with_wall then
            goto continue
        end

        -- Check collision with boxes
        local collided_box = nil
        for _, box in ipairs(boxes) do
            if does_collide(new_player_pos.bound, box.bound) then
                collided_box = box
                break
            end
        end

        if not collided_box then
            player_pos = new_player_pos
            goto continue
        end

        -- Attempt to move the box
        local drafts = {}
        local finalized = {}
        local move_possible = true

        table.insert(drafts, {
            x = collided_box.x + dir.x,
            y = collided_box.y + dir.y,
            bound = create_bound(collided_box.x + dir.x - 1, collided_box.y + dir.y - 1, 2, 1),
            id = collided_box.id
        })

        while #drafts > 0 do
            local draft = table.remove(drafts, 1)

            -- Check collision with walls
            for _, wall in ipairs(walls) do
                if does_collide(draft.bound, wall.bound) then
                    move_possible = false
                    break
                end
            end
            if not move_possible then
                break
            end

            -- Check collision with other boxes
            for _, box in ipairs(boxes) do
                if box.id ~= draft.id and does_collide(draft.bound, box.bound) then
                    table.insert(drafts, {
                        x = box.x + dir.x,
                        y = box.y + dir.y,
                        bound = create_bound(box.x + dir.x - 1, box.y + dir.y - 1, 2, 1),
                        id = box.id
                    })
                end
            end
            table.insert(finalized, draft)
        end

        if move_possible then
            player_pos = new_player_pos
            for _, final in ipairs(finalized) do
                -- Update the box position
                for idx, box in ipairs(boxes) do
                    if box.id == final.id then
                        boxes[idx] = final
                        break
                    end
                end
            end
        end

        ::continue::
    end
end

-- Calculate the final score based on boxes' positions
function calculate_score(boxes)
    local score = 0
    for _, box in ipairs(boxes) do
        score = score + box.bound.y * 100 + box.bound.x
    end
    return score
end

-- Main function
function main()
    local filename = "input_level_15.txt"
    local grid, movements = parse_input(filename)
    local player_pos = find_starting_position(grid)
    local boxes, walls = initialize_objects(grid)

    if not player_pos then
        error("Player starting position not found.")
    end

    process_movements(movements, player_pos, boxes, walls)
    local score = calculate_score(boxes)
    print("Sum of all boxes' final GPS coordinates:", score)
end

-- Run the main function
main()