const fs = require('fs');
const path = require('path');

/**
 * Reads the input file and returns a 2D array representing the grid of tree heights.
 * @param {string} filePath - Path to the input file.
 * @returns {number[][]} - 2D array of tree heights.
 */
function readInput(filePath) {
    const input = fs.readFileSync(filePath, 'utf-8');
    const lines = input.trim().split('\n');
    const grid = lines.map(line => line.trim().split('').map(Number));
    return grid;
}

/**
 * Calculates the viewing distance in a specific direction.
 * @param {number[][]} grid - 2D array of tree heights.
 * @param {number} row - Current tree's row index.
 * @param {number} col - Current tree's column index.
 * @param {string} direction - Direction to look ('up', 'down', 'left', 'right').
 * @returns {number} - Viewing distance in the specified direction.
 */
function getViewingDistance(grid, row, col, direction) {
    const currentHeight = grid[row][col];
    let distance = 0;
    const numRows = grid.length;
    const numCols = grid[0].length;

    switch(direction) {
        case 'up':
            for (let r = row - 1; r >= 0; r--) {
                distance++;
                if (grid[r][col] >= currentHeight) {
                    break;
                }
            }
            break;
        case 'down':
            for (let r = row + 1; r < numRows; r++) {
                distance++;
                if (grid[r][col] >= currentHeight) {
                    break;
                }
            }
            break;
        case 'left':
            for (let c = col - 1; c >= 0; c--) {
                distance++;
                if (grid[row][c] >= currentHeight) {
                    break;
                }
            }
            break;
        case 'right':
            for (let c = col + 1; c < numCols; c++) {
                distance++;
                if (grid[row][c] >= currentHeight) {
                    break;
                }
            }
            break;
        default:
            throw new Error(`Invalid direction: ${direction}`);
    }

    return distance;
}

/**
 * Calculates the scenic score for a tree at position (row, col).
 * @param {number[][]} grid - 2D array of tree heights.
 * @param {number} row - Row index of the tree.
 * @param {number} col - Column index of the tree.
 * @returns {number} - Scenic score of the tree.
 */
function calculateScenicScore(grid, row, col) {
    const up = getViewingDistance(grid, row, col, 'up');
    const down = getViewingDistance(grid, row, col, 'down');
    const left = getViewingDistance(grid, row, col, 'left');
    const right = getViewingDistance(grid, row, col, 'right');
    return up * down * left * right;
}

/**
 * Finds the highest scenic score in the grid.
 * @param {number[][]} grid - 2D array of tree heights.
 * @returns {number} - Highest scenic score found.
 */
function findHighestScenicScore(grid) {
    let maxScenicScore = 0;
    const numRows = grid.length;
    const numCols = grid[0].length;

    // Iterate through each tree in the grid
    for (let row = 0; row < numRows; row++) {
        for (let col = 0; col < numCols; col++) {
            // Skip edge trees as their scenic score will be zero
            if (row === 0 || row === numRows - 1 || col === 0 || col === numCols - 1) {
                continue;
            }

            const scenicScore = calculateScenicScore(grid, row, col);
            if (scenicScore > maxScenicScore) {
                maxScenicScore = scenicScore;
            }
        }
    }

    return maxScenicScore;
}

/**
 * Main function to execute the scenic score calculation.
 */
function main() {
    const inputPath = path.join(__dirname, 'input_level_8.txt');
    const grid = readInput(inputPath);
    const highestScenicScore = findHighestScenicScore(grid);
    console.log(`Highest scenic score possible for any tree: ${highestScenicScore}`);
}

// Execute the main function
main();
