const fs = require('fs');

// Read input from the text file
const input = fs.readFileSync('input_level_6.txt', 'utf-8').trim();

// Function to detect the start-of-message marker
function findStartOfMessageMarker(buffer) {
    for (let i = 13; i < buffer.length; i++) {
        const recentChars = new Set(buffer.slice(i - 13, i + 1));
        if (recentChars.size === 14) {
            return i + 1;  // Return the position (1-based index)
        }
    }
    return -1;  // No marker found
}

// Find the start-of-message marker
const result = findStartOfMessageMarker(input);

console.log(`The first start-of-message marker is detected after ${result} characters.`);
