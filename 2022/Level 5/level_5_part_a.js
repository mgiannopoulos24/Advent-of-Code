const fs = require('fs');

// Read input from the text file
const input = fs.readFileSync('input_level_5.txt', 'utf-8');

// Split input into lines
const lines = input.split('\n');

// Function to parse the initial stack configuration
function parseInitialStacks(lines) {
    const stacks = [];
    let stackLines = [];

    // Collect the lines that represent the initial stacks
    for (let line of lines) {
        if (line.trim() === '') break; // Blank line separates stacks from instructions
        stackLines.push(line);
    }

    // Initialize stacks array based on the number of columns (from the last line of the stack drawing)
    const stackCount = stackLines[stackLines.length - 1].trim().split(/\s+/).length;
    for (let i = 0; i < stackCount; i++) {
        stacks[i] = [];
    }

    // Parse the stacks from bottom to top
    for (let i = stackLines.length - 2; i >= 0; i--) {
        const line = stackLines[i];
        for (let j = 0; j < stackCount; j++) {
            const crate = line[1 + j * 4];
            if (crate !== ' ') {
                stacks[j].push(crate);
            }
        }
    }

    return stacks;
}

// Function to process the rearrangement instructions
function processInstructions(lines, stacks) {
    for (let line of lines) {
        if (line.startsWith('move')) {
            const [_, quantity, from, to] = line.match(/move (\d+) from (\d+) to (\d+)/).map(Number);

            for (let i = 0; i < quantity; i++) {
                const crate = stacks[from - 1].pop();
                stacks[to - 1].push(crate);
            }
        }
    }
}

// Parse the initial stack configuration
const stacks = parseInitialStacks(lines);

// Process the rearrangement instructions
processInstructions(lines, stacks);

// Get the crate on top of each stack
const result = stacks.map(stack => stack[stack.length - 1] || ' ').join('');

console.log(`Crates on top of each stack: ${result}`);