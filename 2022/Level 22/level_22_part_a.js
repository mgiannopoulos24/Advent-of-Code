const fs = require('fs');

function parseInput(input) {
    const [mapSection, pathSection] = input.split('\n\n');
    const mapLines = mapSection.split('\n');
    const path = pathSection.trim();

    // Create 2D map with padding
    const maxWidth = Math.max(...mapLines.map(line => line.length));
    const map = mapLines.map(line => line.padEnd(maxWidth, ' '));

    return { map, path };
}

function findStartingPosition(map) {
    return [0, map[0].indexOf('.')];
}

function parsePathInstructions(path) {
    const instructions = [];
    let currentNumber = '';
    
    for (const char of path) {
        if (/\d/.test(char)) {
            currentNumber += char;
        } else {
            instructions.push(parseInt(currentNumber), char);
            currentNumber = '';
        }
    }
    
    if (currentNumber) {
        instructions.push(parseInt(currentNumber));
    }
    
    return instructions;
}

// Directions: Right, Down, Left, Up
const DIRECTIONS = [
    [0, 1],   // Right
    [1, 0],   // Down
    [0, -1],  // Left
    [-1, 0]   // Up
];

function wrapAround(map, row, col, dirIndex) {
    const [dRow, dCol] = DIRECTIONS[dirIndex];
    let newRow = row, newCol = col;

    // Move in opposite direction until hitting board edge or map boundary
    while (true) {
        const testRow = (newRow - dRow + map.length) % map.length;
        const testCol = (newCol - dCol + map[0].length) % map[0].length;

        if (map[testRow][testCol] === ' ') break;
        
        newRow = testRow;
        newCol = testCol;
    }

    return [newRow, newCol];
}

function moveOnMap(map, startRow, startCol, instructions) {
    let row = startRow, col = startCol;
    let dirIndex = 0; // Initially facing right

    for (const instruction of instructions) {
        if (typeof instruction === 'number') {
            // Move steps
            for (let step = 0; step < instruction; step++) {
                const [dRow, dCol] = DIRECTIONS[dirIndex];
                let newRow = (row + dRow + map.length) % map.length;
                let newCol = (col + dCol + map[0].length) % map[0].length;

                // If new position is outside map or space, wrap around
                if (map[newRow][newCol] === ' ') {
                    [newRow, newCol] = wrapAround(map, row, col, dirIndex);
                }

                // Stop if wall encountered
                if (map[newRow][newCol] === '#') break;

                row = newRow;
                col = newCol;
            }
        } else {
            // Turn
            dirIndex = (dirIndex + (instruction === 'R' ? 1 : -1) + 4) % 4;
        }
    }

    return [row + 1, col + 1, dirIndex];
}

function calculatePassword(row, col, facing) {
    return 1000 * row + 4 * col + facing;
}

function solveMonkeyMap(filename) {
    const input = fs.readFileSync(filename, 'utf8');
    const { map, path } = parseInput(input);
    const [startRow, startCol] = findStartingPosition(map);
    const instructions = parsePathInstructions(path);

    const [row, col, facing] = moveOnMap(map, startRow, startCol, instructions);
    return calculatePassword(row, col, facing);
}

// Example usage
const password = solveMonkeyMap('input_level_22.txt');
console.log('Final Password:', password);

module.exports = { solveMonkeyMap };