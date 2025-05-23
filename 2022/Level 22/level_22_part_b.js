const fs = require('fs');

function parseInput(input) {
    const [mapSection, pathSection] = input.split('\n\n');
    let mapLines = mapSection.split('\n');
    // Do not pad for part two; we need the original spacing.
    return { map: mapLines, path: pathSection.trim() };
}

function parsePathInstructions(path) {

    const tokens = [];
    let currentNumber = '';
    for (const char of path) {
        if (/\d/.test(char)) {
            currentNumber += char;
        } else {
            if (currentNumber) {
                tokens.push(currentNumber);
                currentNumber = '';
            }
            tokens.push(char);
        }
    }
    if (currentNumber) tokens.push(currentNumber);
    return tokens;
}

function getMapMeta(map) {
    const rowStart = [];
    const rowWidth = [];
    const maxWidth = Math.max(...map.map(line => line.length));
    const colStart = new Array(maxWidth).fill(Infinity);
    const colHeight = new Array(maxWidth).fill(0);
    for (let row = 0; row < map.length; row++) {
        const line = map[row];
        let start = 0;
        while (start < line.length && line[start] === ' ') {
            start++;
        }
        rowStart.push(start);
        rowWidth.push(line.length - start);
        for (let col = start; col < line.length; col++) {
            colStart[col] = Math.min(colStart[col], row);
            // Update colHeight: the height is the difference from the first non-space plus one.
            colHeight[col] = Math.max(colHeight[col], row - colStart[col] + 1);
        }
    }
    // Length of cube side.
    const total = rowWidth.reduce((a, b) => a + b, 0);
    const cubeSide = Math.floor(Math.sqrt(total / 6));
    return { rowStart, rowWidth, colStart, colHeight, cubeSide };
}

function mod(n, m) {
    return ((n % m) + m) % m;
}

/*
  The map is not padded so we use our computed metadata.
  The facing is one of "R", "L", "U", or "D".
*/
function moveCube(map, meta, instructions) {
    let row = 0;
    let col = meta.rowStart[0]; // starting position: first non-space of row 0
    let facing = "R";
    const directions = ["U", "R", "D", "L"];
    const moves = { "R": 1, "L": -1, "D": 1, "U": -1 };
  
    for (const token of instructions) {
        if (/^\d+$/.test(token)) {
            const num = parseInt(token, 10);
            for (let i = 0; i < num; i++) {
                let newRow = row, newCol = col, newFacing = facing;
                if (facing === "R" || facing === "L") {
                    const move = moves[facing];
                    newCol = col + move;
                    // Check horizontal boundary on this row.
                    if (facing === "R" && newCol === meta.rowStart[row] + meta.rowWidth[row]) {
                        const band = Math.floor(row / meta.cubeSide);
                        if (band === 0) {
                            newRow = 2 * meta.cubeSide + mod(-row - 1, meta.cubeSide);
                            newCol = 2 * meta.cubeSide - 1;
                            newFacing = "L";
                        } else if (band === 1) {
                            newCol = (row % meta.cubeSide) + 2 * meta.cubeSide;
                            newRow = meta.cubeSide - 1;
                            newFacing = "U";
                        } else if (band === 2) {
                            newRow = mod(-row - 1, meta.cubeSide);
                            newCol = 3 * meta.cubeSide - 1;
                            newFacing = "L";
                        } else if (band === 3) {
                            newCol = meta.cubeSide + (row % meta.cubeSide);
                            newRow = 3 * meta.cubeSide - 1;
                            newFacing = "U";
                        }
                    } else if (facing === "L" && newCol === meta.rowStart[row] - 1) {
                        const band = Math.floor(row / meta.cubeSide);
                        if (band === 0) {
                            newRow = 2 * meta.cubeSide + mod(-row - 1, meta.cubeSide);
                            newCol = 0;
                            newFacing = "R";
                        } else if (band === 1) {
                            newRow = 2 * meta.cubeSide;
                            newCol = row % meta.cubeSide;
                            newFacing = "D";
                        } else if (band === 2) {
                            newRow = mod(-row - 1, meta.cubeSide);
                            newCol = meta.cubeSide;
                            newFacing = "R";
                        } else if (band === 3) {
                            newRow = 0;
                            newCol = meta.cubeSide + (row % meta.cubeSide);
                            newFacing = "D";
                        }
                    }
                } else { // vertical movement ("U" or "D")
                    const move = moves[facing];
                    newRow = row + move;
                    if (facing === "D" && newRow === meta.colStart[col] + meta.colHeight[col]) {
                        const band = Math.floor(col / meta.cubeSide);
                        if (band === 2) {
                            newRow = meta.cubeSide + (col % meta.cubeSide);
                            newCol = 2 * meta.cubeSide - 1;
                            newFacing = "L";
                        } else if (band === 1) {
                            newRow = 3 * meta.cubeSide + (col % meta.cubeSide);
                            newCol = meta.cubeSide - 1;
                            newFacing = "L";
                        } else if (band === 0) {
                            newRow = 0;
                            newCol = 2 * meta.cubeSide + col;
                            newFacing = "D";
                        }
                    } else if (facing === "U" && newRow === meta.colStart[col] - 1) {
                        const band = Math.floor(col / meta.cubeSide);
                        if (band === 0) {
                            newRow = meta.cubeSide + col;
                            newCol = meta.cubeSide;
                            newFacing = "R";
                        } else if (band === 1) {
                            newRow = 3 * meta.cubeSide + (col % meta.cubeSide);
                            newCol = 0;
                            newFacing = "R";
                        } else if (band === 2) {
                            newRow = 4 * meta.cubeSide - 1;
                            newCol = col % meta.cubeSide;
                            newFacing = "U";
                        }
                    }
                }
                // Check if the destination is a wall.
                if (map[newRow][newCol] === '#') break;
                row = newRow;
                col = newCol;
                facing = newFacing;
            }
        } else {
            // Rotate instruction.
            const currInd = directions.indexOf(facing);
            if (token === "R") {
                facing = directions[(currInd + 1) % 4];
            } else {
                facing = directions[(currInd + 3) % 4];
            }
        }
    }
    return { row, col, facing };
}

function calculatePassword(row, col, facing) {
    const facingDict = { "R": 0, "D": 1, "L": 2, "U": 3 };
    return 1000 * (row + 1) + 4 * (col + 1) + facingDict[facing];
}

function solveMonkeyCube(filename) {
    const input = fs.readFileSync(filename, 'utf8');
    const { map, path } = parseInput(input);
    const instructions = parsePathInstructions(path);
    const meta = getMapMeta(map);
    const { row, col, facing } = moveCube(map, meta, instructions);
    return calculatePassword(row, col, facing);
}

const cubePassword = solveMonkeyCube('input.txt');
console.log('Final Cube Password:', cubePassword);

module.exports = { solveMonkeyCube };