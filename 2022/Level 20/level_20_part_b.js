const fs = require('fs');
const path = require('path');

function mixFile(originalOrder, rounds = 1) {
    // Create a list of objects to track original and current positions
    const mixed = originalOrder.map((value, originalIndex) => ({
        value,
        originalIndex
    }));

    const length = mixed.length;

    // Perform multiple mixing rounds
    for (let round = 0; round < rounds; round++) {
        // Mix each number in its original order
        for (let i = 0; i < length; i++) {
            // Find the current index of the number with this original index
            const currentIndex = mixed.findIndex(item => item.originalIndex === i);
            const item = mixed[currentIndex];

            // Remove the item from its current position
            mixed.splice(currentIndex, 1);

            // Calculate new position, using modulo to handle wrapping
            let newIndex = (currentIndex + item.value) % (length - 1);
            
            // Handle negative movements
            if (newIndex <= 0) {
                newIndex += (length - 1);
            }

            // Insert the item at the new position
            mixed.splice(newIndex, 0, item);
        }
    }

    return mixed.map(item => item.value);
}

function findGroveCoordinates(mixedNumbers) {
    // Find the index of 0
    const zeroIndex = mixedNumbers.indexOf(0);

    // Calculate the coordinates
    const coordinates = [
        mixedNumbers[(zeroIndex + 1000) % mixedNumbers.length],
        mixedNumbers[(zeroIndex + 2000) % mixedNumbers.length],
        mixedNumbers[(zeroIndex + 3000) % mixedNumbers.length]
    ];

    return coordinates.reduce((sum, coord) => sum + coord, 0);
}

function solve() {
    const DECRYPTION_KEY = 811589153;
    const inputPath = path.join(__dirname, 'input_level_20.txt');
    
    // Read and apply decryption key
    const input = fs.readFileSync(inputPath, 'utf-8')
        .trim()
        .split('\n')
        .map(Number)
        .map(num => num * DECRYPTION_KEY);

    // Mix 10 times
    const mixedNumbers = mixFile(input, 10);
    const result = findGroveCoordinates(mixedNumbers);

    console.log('Grove Coordinates Sum:', result);
    return result;
}

solve();

module.exports = { mixFile, findGroveCoordinates, solve };