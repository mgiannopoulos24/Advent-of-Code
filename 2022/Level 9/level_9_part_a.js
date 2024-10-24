const fs = require('fs');
const path = require('path');

/**
 * Reads the input file and returns an array of movement instructions.
 * @param {string} filePath - Path to the input file.
 * @returns {Array<{direction: string, steps: number}>} - Array of movement instructions.
 */
function readInput(filePath) {
    const input = fs.readFileSync(filePath, 'utf-8');
    const lines = input.trim().split('\n');
    const instructions = lines.map(line => {
        const [direction, steps] = line.trim().split(' ');
        return { direction, steps: parseInt(steps, 10) };
    });
    return instructions;
}

/**
 * Represents a point on the grid.
 */
class Point {
    constructor(x, y) {
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
     * Calculates the Manhattan distance between this point and another.
     * @param {Point} other - The other point.
     * @returns {number} - The Manhattan distance.
     */
    distance(other) {
        return Math.abs(this.x - other.x) + Math.abs(this.y - other.y);
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
    switch(direction) {
        case 'R':
            return { dx: 1, dy: 0 };
        case 'L':
            return { dx: -1, dy: 0 };
        case 'U':
            return { dx: 0, dy: 1 };
        case 'D':
            return { dx: 0, dy: -1 };
        default:
            throw new Error(`Invalid direction: ${direction}`);
    }
}

/**
 * Determines if the tail needs to move based on the head's position.
 * @param {Point} head - The head's position.
 * @param {Point} tail - The tail's position.
 * @returns {boolean} - True if the tail needs to move, else false.
 */
function shouldMoveTail(head, tail) {
    return head.chebyshevDistance(tail) > 1;
}

/**
 * Moves the tail towards the head by one step in each axis if necessary.
 * @param {Point} head - The head's position.
 * @param {Point} tail - The tail's position.
 */
function moveTail(head, tail) {
    const dx = head.x - tail.x;
    const dy = head.y - tail.y;

    // Normalize the movement to at most 1 step in each direction
    const stepX = dx === 0 ? 0 : dx / Math.abs(dx);
    const stepY = dy === 0 ? 0 : dy / Math.abs(dy);

    tail.move(stepX, stepY);
}

/**
 * Simulates the movement of the rope and counts unique tail positions.
 * @param {Array<{direction: string, steps: number}>} instructions - Movement instructions.
 * @returns {number} - Number of unique positions the tail visited.
 */
function simulateRopeMovement(instructions) {
    const head = new Point(0, 0);
    const tail = new Point(0, 0);

    // Use a Set to store unique tail positions
    const visited = new Set();
    visited.add(tail.toString());

    for (const instr of instructions) {
        const { direction, steps } = instr;
        const { dx, dy } = getDirectionVector(direction);

        for (let i = 0; i < steps; i++) {
            // Move the head
            head.move(dx, dy);

            // Check if the tail needs to move
            if (shouldMoveTail(head, tail)) {
                moveTail(head, tail);
                visited.add(tail.toString());
            }
        }
    }

    return visited.size;
}

/**
 * Main function to execute the simulation.
 */
function main() {
    const inputPath = path.join(__dirname, 'input_level_9.txt');
    const instructions = readInput(inputPath);
    const uniqueTailPositions = simulateRopeMovement(instructions);
    console.log(`Number of unique positions the tail visited at least once: ${uniqueTailPositions}`);
}

// Execute the main function
main();
