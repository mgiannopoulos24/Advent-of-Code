const fs = require('fs');

// Function to parse the input into pairs of packets
function parseInput(input) {
    const pairs = input.split('\n\n');
    return pairs.map(pair => pair.split('\n').map(eval)); // Using eval to parse the string into an array
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
    const pairs = parseInput(input);

    let sum = 0;
    for (let i = 0; i < pairs.length; i++) {
        const [left, right] = pairs[i];
        if (compare(left, right) === -1) {
            sum += (i + 1);
        }
    }

    console.log(`The sum of the indices of the pairs in the right order is: ${sum}`);
}

solve();