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

local function is_clique(graph, nodes)
    -- Check if all nodes in the given set form a clique (complete graph).
    for i = 1, #nodes do
        for j = i + 1, #nodes do
            local a, b = nodes[i], nodes[j]
            local connected = false
            for _, neighbor in ipairs(graph[a]) do
                if neighbor == b then
                    connected = true
                    break
                end
            end
            if not connected then
                return false
            end
        end
    end
    return true
end

local function find_largest_clique(graph)
    -- Find the largest set of fully interconnected computers (a clique).
    local nodes = {}
    for node in pairs(graph) do
        table.insert(nodes, node)
    end

    local largest_clique = {}

    local function backtrack(clique, candidates, depth)
        -- Recursive backtracking to find the largest clique.
        if #clique > #largest_clique then
            largest_clique = { table.unpack(clique) }
        end

        for i = depth, #candidates do
            local candidate = candidates[i]
            table.insert(clique, candidate)
            if is_clique(graph, clique) then
                backtrack(clique, candidates, i + 1)
            end
            table.remove(clique)
        end
    end

    backtrack({}, nodes, 1)
    return largest_clique
end

local function main(file_path)
    local graph = parse_input(file_path)
    local largest_clique = find_largest_clique(graph)

    table.sort(largest_clique)
    local password = table.concat(largest_clique, ",")
    print("Password to get into the LAN party: " .. password)
end

main("input_level_23.txt")

-- 42 seconds