const fs = require('fs');
const path = require('path');

// Path to the input file
const inputPath = path.join(__dirname, 'input_level_3.txt');

// Function to calculate the priority of a given character
function getPriority(char) {
    const code = char.charCodeAt(0);
    if (code >= 97 && code <= 122) { // 'a' - 'z'
        return code - 96;
    } else if (code >= 65 && code <= 90) { // 'A' - 'Z'
        return code - 38;
    } else {
        console.warn(`Invalid character encountered: "${char}". Assigning priority 0.`);
        return 0;
    }
}

// Function to find the common character between two strings
function findCommonCharacter(compartment1, compartment2) {
    const set1 = new Set(compartment1);
    for (let char of compartment2) {
        if (set1.has(char)) {
            return char;
        }
    }
    return null; // If no common character is found
}

// Read the input file
fs.readFile(inputPath, 'utf8', (err, data) => {
    if (err) {
        console.error('Error reading the input file:', err);
        return;
    }

    // Split the input into lines and filter out any empty lines
    const lines = data.split(/\r?\n/).filter(line => line.trim() !== '');

    let totalPrioritySum = 0;
    let rucksackCount = 0;

    lines.forEach((line, index) => {
        const trimmedLine = line.trim();
        const length = trimmedLine.length;

        // Ensure that the rucksack has an even number of items
        if (length % 2 !== 0) {
            console.warn(`Rucksack on line ${index + 1} does not have an even number of items. Skipping this rucksack.`);
            return;
        }

        const half = length / 2;
        const compartment1 = trimmedLine.substring(0, half);
        const compartment2 = trimmedLine.substring(half);

        const commonChar = findCommonCharacter(compartment1, compartment2);

        if (commonChar) {
            const priority = getPriority(commonChar);
            totalPrioritySum += priority;
            rucksackCount++;
            // Uncomment the following line for detailed debugging
            // console.log(`Rucksack ${rucksackCount}: Common Item = "${commonChar}" | Priority = ${priority}`);
        } else {
            console.warn(`No common item found in rucksack on line ${index + 1}.`);
        }
    });

    console.log('Total number of rucksacks processed:', rucksackCount);
    console.log('Sum of the priorities of the common item types:', totalPrioritySum);
});
