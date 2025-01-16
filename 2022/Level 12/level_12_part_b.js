const fs = require('fs');

// Read the input 
const input = fs.readFileSync('input_level_12.txt', 'utf8').trim().split('\n');

// Parse the heightmap
const heightmap = input.map(line => line.split(''));
const rows = heightmap.length;
const cols = heightmap[0].length;

// Find the goal position (E) and all starting positions (a or S)
let end;
const starts = [];
for (let i = 0; i < rows; i++) {
    for (let j = 0; j < cols; j++) {
        if (heightmap[i][j] === 'E') {
            end = [i, j];
            heightmap[i][j] = 'z'; // Set goal elevation to 'z'
        } else if (heightmap[i][j] === 'S' || heightmap[i][j] === 'a') {
            starts.push([i, j]);
            heightmap[i][j] = 'a'; // Set starting elevation to 'a'
        }
    }
}

// Directions for moving up, down, left, and right
const directions = [
    [-1, 0], // Up
    [1, 0],  // Down
    [0, -1], // Left
    [0, 1]   // Right
];

// BFS function to find the shortest path from E to any a
function bfs(end) {
    const queue = [[end[0], end[1], 0]]; // [row, col, steps]
    const visited = new Set();
    visited.add(`${end[0]},${end[1]}`);

    while (queue.length > 0) {
        const [row, col, steps] = queue.shift();

        // Check if the current position is a starting position (a)
        if (heightmap[row][col] === 'a') {
            return steps;
        }

        // Explore all four directions
        for (const [dr, dc] of directions) {
            const newRow = row + dr;
            const newCol = col + dc;

            // Check if the new position is within bounds
            if (newRow >= 0 && newRow < rows && newCol >= 0 && newCol < cols) {
                // Check elevation constraint (reverse: current can be at most 1 higher than neighbor)
                const currentElevation = heightmap[row][col].charCodeAt(0);
                const newElevation = heightmap[newRow][newCol].charCodeAt(0);

                if (currentElevation - newElevation <= 1) {
                    const key = `${newRow},${newCol}`;
                    if (!visited.has(key)) {
                        visited.add(key);
                        queue.push([newRow, newCol, steps + 1]);
                    }
                }
            }
        }
    }

    return -1; // No path found
}

// Run BFS from E
const steps = bfs(end);
console.log(`Fewest steps required: ${steps}`);