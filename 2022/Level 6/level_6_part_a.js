const fs = require('fs');

// Read input from the text file
const input = fs.readFileSync('input_level_6.txt', 'utf-8').trim();

// Function to detect the start-of-packet marker
function findStartOfPacketMarker(buffer) {
    for (let i = 3; i < buffer.length; i++) {
        const recentChars = new Set(buffer.slice(i - 3, i + 1));
        if (recentChars.size === 4) {
            return i + 1;  // Return the position (1-based index)
        }
    }
    return -1;  // No marker found
}

// Find the start-of-packet marker
const result = findStartOfPacketMarker(input);

console.log(`The first start-of-packet marker is detected after ${result} characters.`);
