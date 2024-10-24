const fs = require('fs');
const path = require('path');

// Path to the input file
const inputPath = path.join(__dirname, 'input_level_1.txt');

// Read the input file
fs.readFile(inputPath, 'utf8', (err, data) => {
    if (err) {
        console.error('Error reading the input file:', err);
        return;
    }

    // Split the input into groups separated by blank lines
    const elfGroups = data.split(/\r?\n\r?\n/);

    // Calculate the total Calories for each Elf
    const totalCaloriesPerElf = elfGroups.map(group => {
        // Split each group into individual Calorie values, filter out any empty lines, and sum them
        return group
            .split(/\r?\n/)
            .filter(line => line.trim() !== '')
            .map(Number)
            .reduce((acc, curr) => acc + curr, 0);
    });

    // Sort the total Calories in descending order
    const sortedCalories = totalCaloriesPerElf.sort((a, b) => b - a);

    // Get the top three Calorie counts
    const topThree = sortedCalories.slice(0, 3);

    // Calculate the sum of the top three Calories
    const sumTopThree = topThree.reduce((acc, curr) => acc + curr, 0);

    console.log('The top three Elves are carrying the following Calories:', topThree);
    console.log('The total Calories carried by the top three Elves is:', sumTopThree, 'Calories.');
});
