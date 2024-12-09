local function parse_disk_map(disk_map)
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
    local layout = {}
    local file_id = 0
    local file_positions = {} -- file_id -> {start_index, end_index}
    local pos = 1 -- Lua arrays are 1-indexed
    for _, seg in ipairs(segments) do
        local seg_type, length = seg[1], seg[2]
        if seg_type == "file" then
            -- Append file_id length times
            for _ = 1, length do
                table.insert(layout, tostring(file_id))
            end
            file_positions[file_id] = { pos, pos + length - 1 }
            pos = pos + length
            file_id = file_id + 1
        else
            -- Append '.' for free space
            for _ = 1, length do
                table.insert(layout, ".")
            end
            pos = pos + length
        end
    end
    return layout, file_positions, file_id
end

local function find_free_segments(layout)
    local free_segments = {}
    local i = 1
    local n = #layout
    while i <= n do
        if layout[i] == "." then
            local start = i
            while i <= n and layout[i] == "." do
                i = i + 1
            end
            local finish = i - 1
            table.insert(free_segments, { start, finish })
        else
            i = i + 1
        end
    end
    return free_segments
end

local function move_files(layout, file_positions)
    -- Move files in order of decreasing file ID
    local file_ids = {}
    for fid in pairs(file_positions) do
        table.insert(file_ids, fid)
    end
    table.sort(file_ids, function(a, b) return a > b end)

    for _, fid in ipairs(file_ids) do
        local start, finish = file_positions[fid][1], file_positions[fid][2]
        local file_length = finish - start + 1

        -- Find a free segment that can hold this file to the left of 'start'
        local free_segments = find_free_segments(layout)
        local candidate = nil
        for _, segment in ipairs(free_segments) do
            local fs_start, fs_end = segment[1], segment[2]
            if fs_end < start and (fs_end - fs_start + 1) >= file_length then
                candidate = segment
                break
            end
        end

        if candidate then
            local fs_start, _ = candidate[1], candidate[2]
            -- Move the file to this free segment
            local file_blocks = {}
            for i = start, finish do
                table.insert(file_blocks, layout[i])
                layout[i] = "."
            end
            for i = 1, file_length do
                layout[fs_start + i - 1] = file_blocks[i]
            end
            -- Update file position
            file_positions[fid] = { fs_start, fs_start + file_length - 1 }
        end
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
    local layout, file_positions, file_count = build_layout(segments)
    layout = move_files(layout, file_positions)
    local chksum = compute_checksum(layout)
    print(chksum)
end

main()
