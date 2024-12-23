local function parse_input(file_path)
    -- Parse the input file to create a network graph.
    local graph = {}
    for line in io.lines(file_path) do
        local a, b = line:match("(.-)%-(.+)")
        if a and b then
            graph[a] = graph[a] or {}
            graph[b] = graph[b] or {}
            table.insert(graph[a], b)
            table.insert(graph[b], a)
        end
    end
    return graph
end

local function find_triangles(graph)
    -- Find all sets of three interconnected computers.
    local triangles = {}
    local added = {}

    for node, neighbors in pairs(graph) do
        for i = 1, #neighbors do
            local neighbor1 = neighbors[i]
            for j = i + 1, #neighbors do
                local neighbor2 = neighbors[j]
                if graph[neighbor1] then
                    for _, neighbor in ipairs(graph[neighbor1]) do
                        if neighbor == neighbor2 then
                            local triangle = {node, neighbor1, neighbor2}
                            table.sort(triangle)
                            local key = table.concat(triangle, "-")
                            if not added[key] then
                                table.insert(triangles, triangle)
                                added[key] = true
                            end
                        end
                    end
                end
            end
        end
    end

    return triangles
end

local function filter_triangles(triangles)
    -- Filter triangles to include only those with a name starting with 't'.
    local filtered = {}
    for _, triangle in ipairs(triangles) do
        for _, comp in ipairs(triangle) do
            if comp:sub(1, 1) == 't' then
                table.insert(filtered, triangle)
                break
            end
        end
    end
    return filtered
end

local function main(file_path)
    local graph = parse_input(file_path)
    local triangles = find_triangles(graph)
    local filtered_triangles = filter_triangles(triangles)

    print("Total triangles containing a computer starting with 't': " .. #filtered_triangles)
end

main("input_level_23.txt")
