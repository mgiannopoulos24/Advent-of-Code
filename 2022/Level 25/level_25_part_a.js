const fs = require('fs');
const path = require('path');

// Read the input file
function readInputFile(filename) {
    try {
        return fs.readFileSync(path.join(__dirname, filename), 'utf8').trim();
    } catch (err) {
        console.error('Error reading the input file:', err);
        process.exit(1);
    }
}

// Convert a SNAFU number to decimal
function snafuToDecimal(snafu) {
    let decimal = 0;
    const digitMap = {
        '2': 2,
        '1': 1,
        '0': 0,
        '-': -1,
        '=': -2
    };

    // Process each digit, starting from the rightmost (lowest place value)
    for (let i = 0; i < snafu.length; i++) {
        const digitValue = digitMap[snafu[snafu.length - 1 - i]];
        decimal += digitValue * Math.pow(5, i);
    }

    return decimal;
}

// Convert a decimal number to SNAFU
function decimalToSnafu(decimal) {
    if (decimal === 0) return '0';

    const snafuDigits = [];
    
    while (decimal > 0) {
        // Get remainder when divided by 5 (0-4)
        let remainder = decimal % 5;
        
        // Handle the special mapping of SNAFU digits
        if (remainder <= 2) {
            // Digits 0, 1, and 2 map directly
            snafuDigits.unshift(remainder.toString());
        } else if (remainder === 3) {
            // 3 in decimal becomes '=' in SNAFU with a carry
            snafuDigits.unshift('=');
            decimal += 5; // Carry 1 to the next place
        } else if (remainder === 4) {
            // 4 in decimal becomes '-' in SNAFU with a carry
            snafuDigits.unshift('-');
            decimal += 5; // Carry 1 to the next place
        }
        
        // Integer division by 5 to move to the next place value
        decimal = Math.floor(decimal / 5);
    }

    return snafuDigits.join('');
}

// Sum all SNAFU numbers in the input and return the result in SNAFU format
function sumSnafuNumbers(input) {
    const snafuNumbers = input.split('\n');
    
    // Convert each SNAFU number to decimal, sum them
    let decimalSum = 0;
    for (const snafu of snafuNumbers) {
        decimalSum += snafuToDecimal(snafu);
    }
    
    // Convert the sum back to SNAFU
    return decimalToSnafu(decimalSum);
}

// Main function
function main() {
    const input = readInputFile('input_level_25.txt');
    const snafuSum = sumSnafuNumbers(input);
    
    console.log(`The SNAFU number to supply to Bob's console is: ${snafuSum}`);
}

main();

// Export functions for testing
module.exports = {
    snafuToDecimal,
    decimalToSnafu,
    sumSnafuNumbers
};