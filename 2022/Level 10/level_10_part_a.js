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
    const instructions = lines.map(line => {
        const parts = line.trim().split(' ');
        if (parts[0] === 'addx') {
            return { instruction: 'addx', value: parseInt(parts[1], 10) };
        } else if (parts[0] === 'noop') {
            return { instruction: 'noop', value: null };
        } else {
            throw new Error(`Unknown instruction: ${parts[0]}`);
        }
    });
    return instructions;
}

/**
 * Simulates the CPU execution and calculates the sum of signal strengths.
 * @param {Array<{instruction: string, value: number | null}>} instructions - Array of instructions.
 * @param {Array<number>} checkCycles - Cycles at which to record signal strengths.
 * @returns {number} - Sum of the recorded signal strengths.
 */
function simulateCPU(instructions, checkCycles) {
    let X = 1;
    let cycle = 1;
    let sumSignalStrengths = 0;

    // Create a Set for faster lookup
    const cyclesToCheck = new Set(checkCycles);

    for (const instr of instructions) {
        if (instr.instruction === 'noop') {
            // Process one cycle
            if (cyclesToCheck.has(cycle)) {
                sumSignalStrengths += cycle * X;
            }
            cycle++;
        } else if (instr.instruction === 'addx') {
            // Process two cycles
            for (let i = 0; i < 2; i++) {
                if (cyclesToCheck.has(cycle)) {
                    sumSignalStrengths += cycle * X;
                }
                cycle++;
            }
            // After two cycles, update X
            X += instr.value;
        }
    }

    return sumSignalStrengths;
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

    const checkCycles = [20, 60, 100, 140, 180, 220];
    const sumOfSignalStrengths = simulateCPU(instructions, checkCycles);
    console.log(`Sum of the six signal strengths: ${sumOfSignalStrengths}`);
}

// Execute the main function
main();
