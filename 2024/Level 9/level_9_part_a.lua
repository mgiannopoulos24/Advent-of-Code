local function parse_disk_map(disk_map)
    -- disk_map is a string of digits
    local segments = {}
    for i = 1, #disk_map do
        local ch = tonumber(disk_map:sub(i, i))
        if (i - 1) % 2 == 0 then
            -- Even index: file segment
            table.insert(segments, { "file", ch })
        else
            -- Odd index: free segment
            table.insert(segments, { "free", ch })
        end
    end
    return segments
end

local function build_layout(segments)
    -- Assign file IDs as they appear
    local file_id = 0
    local layout = {}
    local file_id_map = {} -- store {file_id, length}
    for _, seg in ipairs(segments) do
        local seg_type, length = seg[1], seg[2]
        if seg_type == "file" then
            -- Append 'file_id' digit length times
            for _ = 1, length do
                table.insert(layout, tostring(file_id))
            end
            table.insert(file_id_map, { file_id, length })
            file_id = file_id + 1
        else
            -- Free space
            for _ = 1, length do
                table.insert(layout, ".")
            end
        end
    end
    return layout, file_id_map
end

local function compact_disk(layout)
    -- Move file blocks from the end to the leftmost free spot until no '.' between file blocks
    while true do
        -- Find the first '.' from the left
        local gap_index = nil
        for i = 1, #layout do
            if layout[i] == "." then
                gap_index = i
                break
            end
        end

        if not gap_index then
            -- No free space at all, fully compact
            break
        end

        -- Check if there is any file block to the right of this gap
        local file_block_index = nil
        for i = #layout, gap_index + 1, -1 do
            if layout[i] ~= "." then
                file_block_index = i
                break
            end
        end

        if not file_block_index then
            -- No file block to move
            break
        end

        -- Move that file block to the gap
        layout[gap_index] = layout[file_block_index]
        layout[file_block_index] = "."
    end

    return layout
end

local function compute_checksum(layout)
    local checksum = 0
    for pos, block in ipairs(layout) do
        if block:match("%d") then
            local file_id = tonumber(block)
            checksum = checksum + (pos - 1) * file_id
        end
    end
    return checksum
end


local function main()
    
    local file = io.open("input_level_9.txt", "r")
    local disk_map = file:read("*all"):gsub("%s+", "")
    file:close()

    local segments = parse_disk_map(disk_map)
    local layout, file_info = build_layout(segments)
    layout = compact_disk(layout)
    local checksum = compute_checksum(layout)
    print(checksum)
end

main()
