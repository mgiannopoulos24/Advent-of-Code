const fs = require('fs');

// Read the input 
const input = fs.readFileSync('input_level_12.txt', 'utf8').trim().split('\n');

// Parse the heightmap
const heightmap = input.map(line => line.split(''));
const rows = heightmap.length;
const cols = heightmap[0].length;

// Find the starting position (S) and the goal position (E)
let start, end;
for (let i = 0; i < rows; i++) {
    for (let j = 0; j < cols; j++) {
        if (heightmap[i][j] === 'S') {
            start = [i, j];
            heightmap[i][j] = 'a'; // Set starting elevation to 'a'
        } else if (heightmap[i][j] === 'E') {
            end = [i, j];
            heightmap[i][j] = 'z'; // Set goal elevation to 'z'
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

// BFS function to find the shortest path
function bfs(start, end) {
    const queue = [[start[0], start[1], 0]]; // [row, col, steps]
    const visited = new Set();
    visited.add(`${start[0]},${start[1]}`);

    while (queue.length > 0) {
        const [row, col, steps] = queue.shift();

        // Check if the current position is the goal
        if (row === end[0] && col === end[1]) {
            return steps;
        }

        // Explore all four directions
        for (const [dr, dc] of directions) {
            const newRow = row + dr;
            const newCol = col + dc;

            // Check if the new position is within bounds
            if (newRow >= 0 && newRow < rows && newCol >= 0 && newCol < cols) {
                // Check elevation constraint
                const currentElevation = heightmap[row][col].charCodeAt(0);
                const newElevation = heightmap[newRow][newCol].charCodeAt(0);

                if (newElevation - currentElevation <= 1) {
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

// Run BFS
const steps = bfs(start, end);
console.log(`Fewest steps required: ${steps}`);