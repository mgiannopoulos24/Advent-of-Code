const fs = require('fs');

// Read input from the text file
const input = fs.readFileSync('input_level_4.txt', 'utf-8');

// Split input into lines
const assignmentPairs = input.trim().split('\n');

// Function to check if two ranges overlap
function isOverlapping(range1, range2) {
    const [start1, end1] = range1;
    const [start2, end2] = range2;
    return !(end1 < start2 || end2 < start1);
}

// Process each assignment pair
let overlappingCount = 0;
assignmentPairs.forEach(pair => {
    const [elf1, elf2] = pair.split(',').map(range => range.split('-').map(Number));
    
    if (isOverlapping(elf1, elf2)) {
        overlappingCount++;
    }
});

console.log(`Total overlapping pairs: ${overlappingCount}`);
