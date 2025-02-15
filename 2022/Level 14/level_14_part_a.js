const fs = require('fs');

// Parse the input file and extract rock paths
function parseInput(filePath) {
    const data = fs.readFileSync(filePath, 'utf-8');
    const lines = data.split('\n');
    
    const rocks = [];
    for (const line of lines) {
        const points = line.trim().split(' -> ');
        const rockPath = [];
        for (const point of points) {
            const [x, y] = point.split(',').map(Number);
            rockPath.push({ x, y });
        }
        rocks.push(rockPath);
    }
    
    return rocks;
}

// Initialize the grid with rock structures and find the maximum y-coordinate
function initializeGrid(rocks) {
    const grid = new Map();
    let maxY = 0;
    
    for (const rockPath of rocks) {
        for (let i = 0; i < rockPath.length - 1; i++) {
            const { x: x1, y: y1 } = rockPath[i];
            const { x: x2, y: y2 } = rockPath[i + 1];
            
            if (x1 === x2) {
                for (let y = Math.min(y1, y2); y <= Math.max(y1, y2); y++) {
                    grid.set(`${x1},${y}`, '#');
                }
            } else {
                for (let x = Math.min(x1, x2); x <= Math.max(x1, x2); x++) {
                    grid.set(`${x},${y1}`, '#');
                }
            }
            
            maxY = Math.max(maxY, y1, y2);
        }
    }
    
    return { grid, maxY };
}

// Simulate the falling sand and count the units that come to rest
function simulateSand(grid, maxY) {
    let sandCount = 0;
    while (true) {
        let x = 500, y = 0;
        while (y <= maxY) {
            if (!grid.has(`${x},${y + 1}`)) {
                y++;
            } else if (!grid.has(`${x - 1},${y + 1}`)) {
                x--;
                y++;
            } else if (!grid.has(`${x + 1},${y + 1}`)) {
                x++;
                y++;
            } else {
                grid.set(`${x},${y}`, 'o');
                sandCount++;
                break;
            }
        }
        if (y > maxY) break;
    }
    return sandCount;
}

// Main function
function main() {
    const filePath = 'input_level_14.txt';
    const rocks = parseInput(filePath);
    const { grid, maxY } = initializeGrid(rocks);
    const sandCount = simulateSand(grid, maxY);
    console.log(`Units of sand that come to rest: ${sandCount}`);
}

main();