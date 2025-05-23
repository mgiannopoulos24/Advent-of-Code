const fs = require('fs');
const path = require('path');

// Read the input file
function readInputFile(filename) {
    try {
        return fs.readFileSync(path.join(__dirname, filename), 'utf8').trim();
    } catch (err) {
        console.error('Error reading the input file:', err);
        process.exit(1);
    }
}

// Parse input to get elf positions
function parseInput(input) {
    const elves = new Set();
    const lines = input.split('\n');
    
    for (let y = 0; y < lines.length; y++) {
        for (let x = 0; x < lines[y].length; x++) {
            if (lines[y][x] === '#') {
                elves.add(`${x},${y}`);
            }
        }
    }
    
    return elves;
}

// Check if an elf has neighbors in the specified positions
function hasNeighborsInPositions(elves, x, y, positions) {
    return positions.some(([dx, dy]) => elves.has(`${x + dx},${y + dy}`));
}

// Check if an elf has any neighbors
function hasAnyNeighbors(elves, x, y) {
    const directions = [
        [-1, -1], [0, -1], [1, -1], // NW, N, NE
        [-1, 0], [1, 0],           // W, E
        [-1, 1], [0, 1], [1, 1]    // SW, S, SE
    ];
    
    return hasNeighborsInPositions(elves, x, y, directions);
}

// Simulate one round of elf movement
function simulateRound(elves, roundNum) {
    const directionChecks = [
        { dir: [0, -1], check: [[-1, -1], [0, -1], [1, -1]] }, // N, NW, NE
        { dir: [0, 1], check: [[-1, 1], [0, 1], [1, 1]] },    // S, SW, SE
        { dir: [-1, 0], check: [[-1, -1], [-1, 0], [-1, 1]] }, // W, NW, SW
        { dir: [1, 0], check: [[1, -1], [1, 0], [1, 1]] }     // E, NE, SE
    ];
    
    // Rotate the direction order based on round number
    const rotatedDirections = [...directionChecks];
    for (let i = 0; i < roundNum % 4; i++) {
        rotatedDirections.push(rotatedDirections.shift());
    }
    
    // First half: propose moves
    const proposals = new Map();
    const proposalCounts = new Map();
    
    for (const elfPos of elves) {
        const [x, y] = elfPos.split(',').map(Number);
        
        // Skip if no neighbors
        if (!hasAnyNeighbors(elves, x, y)) {
            continue;
        }
        
        // Try each direction in order
        for (const { dir, check } of rotatedDirections) {
            if (!hasNeighborsInPositions(elves, x, y, check)) {
                const newPos = `${x + dir[0]},${y + dir[1]}`;
                proposals.set(elfPos, newPos);
                
                // Count proposals for each destination
                proposalCounts.set(newPos, (proposalCounts.get(newPos) || 0) + 1);
                break;
            }
        }
    }
    
    // Second half: move elves
    const newElves = new Set(elves);
    let moved = false;
    
    for (const [elfPos, newPos] of proposals.entries()) {
        // Only move if this is the only elf proposing this position
        if (proposalCounts.get(newPos) === 1) {
            newElves.delete(elfPos);
            newElves.add(newPos);
            moved = true;
        }
    }
    
    return { elves: newElves, moved };
}

// Calculate the empty ground tiles in the smallest rectangle containing all elves
function countEmptyGroundTiles(elves) {
    let minX = Infinity;
    let maxX = -Infinity;
    let minY = Infinity;
    let maxY = -Infinity;
    
    // Find the boundaries of the rectangle
    for (const elfPos of elves) {
        const [x, y] = elfPos.split(',').map(Number);
        minX = Math.min(minX, x);
        maxX = Math.max(maxX, x);
        minY = Math.min(minY, y);
        maxY = Math.max(maxY, y);
    }
    
    // Calculate rectangle area and subtract elf count
    const width = maxX - minX + 1;
    const height = maxY - minY + 1;
    const area = width * height;
    
    return area - elves.size;
}

// Solve part A: Simulate 10 rounds and count empty tiles
function solvePartA(input) {
    let elves = parseInput(input);
    
    for (let round = 0; round < 10; round++) {
        const result = simulateRound(elves, round);
        elves = result.elves;
    }
    
    return countEmptyGroundTiles(elves);
}

// Main function
function main() {
    const input = readInputFile('input_level_23.txt');
    const emptyTiles = solvePartA(input);
    console.log(`Empty ground tiles in the smallest rectangle after 10 rounds: ${emptyTiles}`);
}

main();