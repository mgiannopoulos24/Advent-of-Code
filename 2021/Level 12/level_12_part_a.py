from collections import defaultdict

def read_input(filename):
    graph = defaultdict(list)
    with open(filename, 'r') as file:
        lines = [line.strip() for line in file.readlines()]
    for line in lines:
        a, b = line.split('-')
        graph[a].append(b)
        graph[b].append(a)
    return graph

def is_small_cave(cave):
    return cave.islower()

def find_all_paths(graph, cave, visited, path, all_paths):
    if cave == 'end':
        all_paths.append(path)
        return
    
    # If it's a small cave, mark it as visited
    if is_small_cave(cave):
        visited.add(cave)

    # Explore all connected caves
    for neighbor in graph[cave]:
        if neighbor not in visited:
            find_all_paths(graph, neighbor, visited.copy(), path + [neighbor], all_paths)

def find_paths(graph):
    all_paths = []
    visited = set()
    find_all_paths(graph, 'start', visited, ['start'], all_paths)
    return all_paths

if __name__ == "__main__":
    # Read input and create graph
    graph = read_input('input_level_12.txt')
    
    # Find all valid paths
    paths = find_paths(graph)
    
    # Print the number of distinct paths
    print(f"Number of distinct paths: {len(paths)}")
