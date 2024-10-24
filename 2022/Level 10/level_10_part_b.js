const fs = require('fs');
const path = require('path');

/**
 * Reads the input file and returns an array of instructions.
 * @param {string} filePath - Path to the input file.
 * @returns {Array<{instruction: string, value: number | null}>} - Array of instructions.
 */
function readInput(filePath) {
    const input = fs.readFileSync(filePath, 'utf-8');
    const lines = input.trim().split('\n');
    const instructions = lines.map((line, index) => {
        const parts = line.trim().split(' ');
        if (parts[0].toLowerCase() === 'addx') {
            if (parts.length !== 2) {
                throw new Error(`Invalid addx instruction format at line ${index + 1}: "${line}"`);
            }
            const value = parseInt(parts[1], 10);
            if (isNaN(value)) {
                throw new Error(`Invalid addx value at line ${index + 1}: "${line}"`);
            }
            return { instruction: 'addx', value };
        } else if (parts[0].toLowerCase() === 'noop') {
            return { instruction: 'noop', value: null };
        } else {
            throw new Error(`Unknown instruction "${parts[0]}" at line ${index + 1}`);
        }
    });
    return instructions;
}

/**
 * Represents a point on the grid.
 */
class Point {
    constructor(x = 0, y = 0) {
        this.x = x;
        this.y = y;
    }

    /**
     * Moves the point by dx and dy.
     * @param {number} dx - Change in x.
     * @param {number} dy - Change in y.
     */
    move(dx, dy) {
        this.x += dx;
        this.y += dy;
    }

    /**
     * Calculates the Chebyshev distance between this point and another.
     * @param {Point} other - The other point.
     * @returns {number} - The Chebyshev distance.
     */
    chebyshevDistance(other) {
        return Math.max(Math.abs(this.x - other.x), Math.abs(this.y - other.y));
    }

    /**
     * Returns a string representation of the point.
     * @returns {string} - String in the format "x,y".
     */
    toString() {
        return `${this.x},${this.y}`;
    }
}

/**
 * Determines the direction vector based on the given direction character.
 * @param {string} direction - 'R', 'L', 'U', 'D'.
 * @returns {{dx: number, dy: number}} - The direction vector.
 */
function getDirectionVector(direction) {
    switch(direction.toUpperCase()) {
        case 'R':
            return { dx: 1, dy: 0 };
        case 'L':
            return { dx: -1, dy: 0 };
        case 'U':
            return { dx: 0, dy: -1 };
        case 'D':
            return { dx: 0, dy: 1 };
        default:
            throw new Error(`Invalid direction: ${direction}`);
    }
}

/**
 * Simulates the CPU and CRT execution, rendering the screen.
 * @param {Array<{instruction: string, value: number | null}>} instructions - Array of instructions.
 * @param {number} screenWidth - Width of the CRT screen.
 * @param {number} screenHeight - Height of the CRT screen.
 * @returns {string[]} - Array of strings representing each row of the CRT screen.
 */
function simulateCRT(instructions, screenWidth = 40, screenHeight = 6) {
    let X = 1; // Register X starts at 1
    let cycle = 1; // Cycle counter starts at 1

    // Initialize the CRT screen as an array of strings
    const screen = Array.from({ length: screenHeight }, () => '');

    for (const instr of instructions) {
        if (instr.instruction === 'noop') {
            // Process one cycle
            renderPixel(screen, cycle, X, screenWidth, screenHeight);
            cycle++;
        } else if (instr.instruction === 'addx') {
            // Process two cycles
            for (let i = 0; i < 2; i++) {
                renderPixel(screen, cycle, X, screenWidth, screenHeight);
                cycle++;
            }
            // After two cycles, update X
            X += instr.value;
        }
    }

    // Continue rendering until all pixels are drawn
    while (cycle <= screenWidth * screenHeight) {
        renderPixel(screen, cycle, X, screenWidth, screenHeight);
        cycle++;
    }

    return screen;
}

/**
 * Renders a single pixel on the CRT screen based on the current cycle and X register.
 * @param {string[]} screen - Array of strings representing the CRT screen.
 * @param {number} cycle - Current cycle number.
 * @param {number} X - Current value of register X.
 * @param {number} screenWidth - Width of the CRT screen.
 * @param {number} screenHeight - Height of the CRT screen.
 */
function renderPixel(screen, cycle, X, screenWidth, screenHeight) {
    const pixelIndex = cycle - 1;
    const row = Math.floor(pixelIndex / screenWidth);
    const col = pixelIndex % screenWidth;

    if (row >= screenHeight) {
        // Exceeded screen dimensions; do nothing
        return;
    }

    // Sprite covers positions X-1, X, X+1
    if (col >= (X - 1) && col <= (X + 1)) {
        screen[row] += '#';
    } else {
        screen[row] += '.';
    }
}

/**
 * Main function to execute the simulation.
 */
function main() {
    const inputPath = path.join(__dirname, 'input_level_10.txt');
    let instructions;
    try {
        instructions = readInput(inputPath);
    } catch (error) {
        console.error(`Error reading input: ${error.message}`);
        return;
    }

    const screenWidth = 40;
    const screenHeight = 6;
    const renderedScreen = simulateCRT(instructions, screenWidth, screenHeight);

    console.log('CRT Screen Output:');
    renderedScreen.forEach(row => console.log(row));
}

main();
