const fs = require('fs');

// Function to parse the input into an array of packets
function parseInput(input) {
    return input
        .split('\n')
        .filter(line => line.trim() !== '') // Remove blank lines
        .map(eval); // Use eval to parse the string into an array
}

// Function to compare two packets
function compare(left, right) {
    if (typeof left === 'number' && typeof right === 'number') {
        if (left < right) return -1;
        if (left > right) return 1;
        return 0;
    }

    if (Array.isArray(left) && Array.isArray(right)) {
        for (let i = 0; i < Math.min(left.length, right.length); i++) {
            const result = compare(left[i], right[i]);
            if (result !== 0) return result;
        }
        if (left.length < right.length) return -1;
        if (left.length > right.length) return 1;
        return 0;
    }

    if (typeof left === 'number') {
        return compare([left], right);
    }

    if (typeof right === 'number') {
        return compare(left, [right]);
    }

    return 0;
}

function solve() {
    const input = fs.readFileSync('input.txt', 'utf8').trim();
    const packets = parseInput(input);

    // Add the two divider packets
    const divider1 = [[2]];
    const divider2 = [[6]];
    packets.push(divider1, divider2);

    // Sort all packets using the compare function
    packets.sort(compare);

    // Find the indices of the divider packets
    const index1 = packets.findIndex(packet => JSON.stringify(packet) === JSON.stringify(divider1)) + 1;
    const index2 = packets.findIndex(packet => JSON.stringify(packet) === JSON.stringify(divider2)) + 1;

    // Calculate the decoder key
    const decoderKey = index1 * index2;

    console.log(`The decoder key for the distress signal is: ${decoderKey}`);
}

solve();