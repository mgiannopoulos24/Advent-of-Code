const fs = require('fs');
const path = require('path');

// Path to the input file
const inputPath = path.join(__dirname, 'input_level_3.txt');

/**
 * Function to calculate the priority of a given character
 * Lowercase a-z: 1-26
 * Uppercase A-Z: 27-52
 * @param {string} char - Single character representing the item type
 * @returns {number} - Priority of the item
 */
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

/**
 * Function to find the common character among three strings
 * @param {string} str1 - First rucksack's items
 * @param {string} str2 - Second rucksack's items
 * @param {string} str3 - Third rucksack's items
 * @returns {string|null} - The common item type or null if not found
 */
function findCommonBadge(str1, str2, str3) {
    const set1 = new Set(str1);
    const set2 = new Set(str2);
    for (let char of str3) {
        if (set1.has(char) && set2.has(char)) {
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

    // Check if the number of rucksacks is a multiple of three
    if (lines.length % 3 !== 0) {
        console.warn(`Total number of rucksacks (${lines.length}) is not a multiple of three. The last group may be incomplete and will be skipped.`);
    }

    let totalPrioritySum = 0;
    let groupCount = 0;

    // Process rucksacks in groups of three
    for (let i = 0; i < lines.length; i += 3) {
        const group = lines.slice(i, i + 3);
        
        // If the group is incomplete, skip processing
        if (group.length < 3) {
            console.warn(`Incomplete group detected starting at line ${i + 1}. Skipping this group.`);
            continue;
        }

        const [rucksack1, rucksack2, rucksack3] = group;

        const commonBadge = findCommonBadge(rucksack1, rucksack2, rucksack3);

        if (commonBadge) {
            const priority = getPriority(commonBadge);
            totalPrioritySum += priority;
            groupCount++;
            // Uncomment the following line for detailed debugging
            // console.log(`Group ${groupCount}: Common Badge = "${commonBadge}" | Priority = ${priority}`);
        } else {
            console.warn(`No common badge found for group starting at line ${i + 1}.`);
        }
    }

    console.log('Total number of groups processed:', groupCount);
    console.log('Sum of the priorities of the badge item types:', totalPrioritySum);
});
