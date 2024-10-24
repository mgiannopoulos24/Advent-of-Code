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
        return { direction: direction.toUpperCase(), steps: parseInt(steps, 10) };
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
 * Moves a knot towards the target knot by one step.
 * @param {Point} target - The knot to move towards.
 * @param {Point} knot - The current knot to move.
 */
function moveKnotTowards(target, knot) {
    const dx = target.x - knot.x;
    const dy = target.y - knot.y;

    // Determine the direction to move: can be -1, 0, or 1 for both axes
    const stepX = dx === 0 ? 0 : dx / Math.abs(dx);
    const stepY = dy === 0 ? 0 : dy / Math.abs(dy);

    knot.move(stepX, stepY);
}

/**
 * Simulates the movement of the rope and counts unique tail positions.
 * @param {Array<{direction: string, steps: number}>} instructions - Movement instructions.
 * @returns {number} - Number of unique positions the tail visited.
 */
function simulateRopeMovement(instructions) {
    // Initialize ten knots: H, 1, 2, ..., 9
    const knots = Array.from({ length: 10 }, () => new Point());

    // Use a Set to store unique tail (knot 9) positions
    const visited = new Set();
    visited.add(knots[9].toString());

    for (const instr of instructions) {
        const { direction, steps } = instr;
        let directionVector;

        try {
            directionVector = getDirectionVector(direction);
        } catch (error) {
            console.error(error.message);
            continue; // Skip invalid directions
        }

        for (let i = 0; i < steps; i++) {
            // Move the head (knot 0)
            knots[0].move(directionVector.dx, directionVector.dy);

            // Now, move each subsequent knot if necessary
            for (let j = 1; j < knots.length; j++) {
                const prevKnot = knots[j - 1];
                const currentKnot = knots[j];

                // Check if current knot is adjacent to the previous knot
                if (currentKnot.chebyshevDistance(prevKnot) > 1) {
                    moveKnotTowards(prevKnot, currentKnot);
                } else {
                    // If the knot is adjacent, no need to move further knots
                    break;
                }
            }

            // Record the tail's position
            visited.add(knots[9].toString());
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
