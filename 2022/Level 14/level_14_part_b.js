const fs = require('fs');

function parseInput(filePath) {
    const lines = fs.readFileSync(filePath, 'utf-8').trim().split('\n');
    let rocks = [];
    
    for (let line of lines) {
        const points = line.trim().split(' -> ');
        let rockPath = [];
        
        for (let point of points) {
            const [x, y] = point.split(',').map(Number);
            rockPath.push([x, y]);
        }
        
        rocks.push(rockPath);
    }
    
    return rocks;
}

function initializeGrid(rocks) {
    let grid = new Map();
    let maxY = 0;
    
    for (let rockPath of rocks) {
        for (let i = 0; i < rockPath.length - 1; i++) {
            let [x1, y1] = rockPath[i];
            let [x2, y2] = rockPath[i + 1];
            
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
    
    return [grid, maxY + 2];
}

function simulateSand(grid, floorY) {
    let sandCount = 0;
    
    while (true) {
        let x = 500, y = 0;
        
        if (grid.has(`${x},${y}`)) {
            break;
        }
        
        while (y < floorY - 1) {
            if (!grid.has(`${x},${y + 1}`)) {
                y += 1;
            } else if (!grid.has(`${x - 1},${y + 1}`)) {
                x -= 1;
                y += 1;
            } else if (!grid.has(`${x + 1},${y + 1}`)) {
                x += 1;
                y += 1;
            } else {
                break;
            }
        }
        
        grid.set(`${x},${y}`, 'o');
        sandCount++;
    }
    
    return sandCount;
}

function main() {
    const filePath = 'input_level_14.txt';
    const rocks = parseInput(filePath);
    const [grid, floorY] = initializeGrid(rocks);
    const sandCount = simulateSand(grid, floorY);
    console.log(`Units of sand that come to rest: ${sandCount}`);
}

main();