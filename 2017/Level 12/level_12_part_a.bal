import ballerina/io;
import ballerina/regex;

public function main() {
    // Read input from the file and build the adjacency list
    map<int[]> adjacencyList = readInputAsAdjacencyList("input_level_12.txt");

    // Set to track visited nodes
    map<boolean> visited = {};

    // Perform BFS to find all programs in the group containing program 0
    int groupSize = bfs(0, adjacencyList, visited);

    // Output the result
    io:println("Number of programs in the group that contains program 0: ", groupSize);
}

// BFS function to find the group size for a given starting node
function bfs(int startNode, map<int[]> adjacencyList, map<boolean> visited) returns int {
    // Queue for BFS
    int[] queue = [startNode];
    // Mark the starting node as visited
    visited[startNode.toString()] = true;
    int groupSize = 0;

    // BFS loop
    while queue.length() > 0 {
        int currentNode = queue.shift();
        groupSize += 1;

        // Get the neighbors of the current node, handle optional type with default value
        int[] neighbors = adjacencyList[currentNode.toString()] ?: [];

        foreach int neighbor in neighbors {
            if !visited.hasKey(neighbor.toString()) {
                // Mark the neighbor as visited and add it to the queue
                visited[neighbor.toString()] = true;
                queue.push(neighbor);
            }
        }
    }

    return groupSize;
}

// Function to read the input from a file and build the adjacency list
function readInputAsAdjacencyList(string filePath) returns map<int[]> {
    string content = checkpanic io:fileReadString(filePath);
    string[] lines = regex:split(content, "\n");

    map<int[]> adjacencyList = {};

    foreach string line in lines {
        if line.trim().length() > 0 {
            // Split the line into program ID and its connections
            string[] parts = regex:split(line, " <-> ");
            int programId = checkpanic int:fromString(parts[0].trim());
            string[] connections = regex:split(parts[1].trim(), ", ");
            int[] neighbors = from string conn in connections
                              select checkpanic int:fromString(conn.trim());

            adjacencyList[programId.toString()] = neighbors;
        }
    }

    return adjacencyList;
}
