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
 * Checks if a tree at position (row, col) is visible from the left.
 * @param {number[][]} grid - 2D array of tree heights.
 * @param {number} row - Row index of the tree.
 * @param {number} col - Column index of the tree.
 * @returns {boolean} - True if visible from the left, else false.
 */
function isVisibleLeft(grid, row, col) {
    const currentHeight = grid[row][col];
    for (let c = 0; c < col; c++) {
        if (grid[row][c] >= currentHeight) {
            return false;
        }
    }
    return true;
}

/**
 * Checks if a tree at position (row, col) is visible from the right.
 * @param {number[][]} grid - 2D array of tree heights.
 * @param {number} row - Row index of the tree.
 * @param {number} col - Column index of the tree.
 * @returns {boolean} - True if visible from the right, else false.
 */
function isVisibleRight(grid, row, col) {
    const currentHeight = grid[row][col];
    const lastCol = grid[0].length - 1;
    for (let c = col + 1; c <= lastCol; c++) {
        if (grid[row][c] >= currentHeight) {
            return false;
        }
    }
    return true;
}

/**
 * Checks if a tree at position (row, col) is visible from the top.
 * @param {number[][]} grid - 2D array of tree heights.
 * @param {number} row - Row index of the tree.
 * @param {number} col - Column index of the tree.
 * @returns {boolean} - True if visible from the top, else false.
 */
function isVisibleTop(grid, row, col) {
    const currentHeight = grid[row][col];
    for (let r = 0; r < row; r++) {
        if (grid[r][col] >= currentHeight) {
            return false;
        }
    }
    return true;
}

/**
 * Checks if a tree at position (row, col) is visible from the bottom.
 * @param {number[][]} grid - 2D array of tree heights.
 * @param {number} row - Row index of the tree.
 * @param {number} col - Column index of the tree.
 * @returns {boolean} - True if visible from the bottom, else false.
 */
function isVisibleBottom(grid, row, col) {
    const currentHeight = grid[row][col];
    const lastRow = grid.length - 1;
    for (let r = row + 1; r <= lastRow; r++) {
        if (grid[r][col] >= currentHeight) {
            return false;
        }
    }
    return true;
}


function isTreeVisible(grid, row, col) {
    // If the tree is on the edge, it is visible
    const numRows = grid.length;
    const numCols = grid[0].length;
    if (row === 0 || row === numRows - 1 || col === 0 || col === numCols - 1) {
        return true;
    }

    // Check visibility in all four directions
    return (
        isVisibleLeft(grid, row, col) ||
        isVisibleRight(grid, row, col) ||
        isVisibleTop(grid, row, col) ||
        isVisibleBottom(grid, row, col)
    );
}

function countVisibleTrees(grid) {
    let visibleCount = 0;
    const numRows = grid.length;
    const numCols = grid[0].length;

    for (let row = 0; row < numRows; row++) {
        for (let col = 0; col < numCols; col++) {
            if (isTreeVisible(grid, row, col)) {
                visibleCount++;
            }
        }
    }

    return visibleCount;
}

function main() {
    const inputPath = path.join(__dirname, 'input_level_8.txt');
    const grid = readInput(inputPath);
    const visibleTrees = countVisibleTrees(grid);
    console.log(`Total number of visible trees: ${visibleTrees}`);
}

// Execute the main function
main();
