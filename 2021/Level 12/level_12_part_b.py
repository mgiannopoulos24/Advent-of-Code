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

def find_all_paths(graph, cave, visited, path, all_paths, visited_twice):
    if cave == 'end':
        all_paths.append(path)
        return
    
    # If it's a small cave, mark it as visited
    if is_small_cave(cave):
        visited[cave] += 1

    # Explore all connected caves
    for neighbor in graph[cave]:
        if neighbor == 'start':
            continue  # We can't visit 'start' again
        
        if neighbor == 'end' or neighbor not in visited or visited[neighbor] == 0:
            # If it's 'end' or an unvisited cave, continue exploring
            find_all_paths(graph, neighbor, visited.copy(), path + [neighbor], all_paths, visited_twice)
        elif is_small_cave(neighbor) and visited[neighbor] == 1 and not visited_twice:
            # If we haven't visited any small cave twice and this cave was only visited once, visit it again
            find_all_paths(graph, neighbor, visited.copy(), path + [neighbor], all_paths, True)
    
    # Backtrack: unmark the cave visit
    if is_small_cave(cave):
        visited[cave] -= 1

def find_paths(graph):
    all_paths = []
    visited = defaultdict(int)
    find_all_paths(graph, 'start', visited, ['start'], all_paths, False)
    return all_paths

if __name__ == "__main__":
    # Read input and create graph
    graph = read_input('input_level_12.txt')
    
    # Find all valid paths
    paths = find_paths(graph)
    
    # Print the number of distinct paths
    print(f"Number of distinct paths: {len(paths)}")
