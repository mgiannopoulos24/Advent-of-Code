const fs = require('fs');

// Read the input 
const input = fs.readFileSync('input_level_11.txt', 'utf8').split('\n\n');

// Parse the input to create monkey objects
const monkeys = input.map(monkeyData => {
    const lines = monkeyData.split('\n');
    const startingItems = lines[1].split(': ')[1].split(', ').map(Number);
    const operation = lines[2].split('= ')[1];
    const testDivisor = parseInt(lines[3].split('by ')[1]);
    const trueMonkey = parseInt(lines[4].split('monkey ')[1]);
    const falseMonkey = parseInt(lines[5].split('monkey ')[1]);

    return {
        items: startingItems,
        operation,
        testDivisor,
        trueMonkey,
        falseMonkey,
        inspections: 0
    };
});

// Calculate the least common multiple (LCM) of all test divisors
const lcm = monkeys.reduce((acc, monkey) => acc * monkey.testDivisor, 1);

// Simulate 10,000 rounds
for (let round = 0; round < 10000; round++) {
    for (let i = 0; i < monkeys.length; i++) {
        const monkey = monkeys[i];
        while (monkey.items.length > 0) {
            let item = monkey.items.shift();
            monkey.inspections++;

            // Perform the operation
            let newWorryLevel = eval(monkey.operation.replace(/old/g, item));

            // Use modulo to keep worry levels manageable
            newWorryLevel = newWorryLevel % lcm;

            // Test and throw the item
            if (newWorryLevel % monkey.testDivisor === 0) {
                monkeys[monkey.trueMonkey].items.push(newWorryLevel);
            } else {
                monkeys[monkey.falseMonkey].items.push(newWorryLevel);
            }
        }
    }
}

// Calculate the level of monkey business
const inspections = monkeys.map(monkey => monkey.inspections).sort((a, b) => b - a);
const monkeyBusiness = inspections[0] * inspections[1];

console.log(`Level of monkey business after 10000 rounds: ${monkeyBusiness}`);