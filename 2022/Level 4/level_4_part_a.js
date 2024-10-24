const fs = require('fs');

// Read input from the text file
const input = fs.readFileSync('input_level_4.txt', 'utf-8');

// Split input into lines
const assignmentPairs = input.trim().split('\n');

// Function to check if one range fully contains the other
function isFullyContained(range1, range2) {
    const [start1, end1] = range1;
    const [start2, end2] = range2;
    return (start1 <= start2 && end1 >= end2) || (start2 <= start1 && end2 >= end1);
}

// Process each assignment pair
let fullyContainedCount = 0;
assignmentPairs.forEach(pair => {
    const [elf1, elf2] = pair.split(',').map(range => range.split('-').map(Number));
    
    if (isFullyContained(elf1, elf2)) {
        fullyContainedCount++;
    }
});

console.log(`Total fully contained pairs: ${fullyContainedCount}`);
