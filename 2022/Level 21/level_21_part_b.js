const fs = require('fs');

class Monkey {
    constructor(job) {
        this.job = job;
        this.value = null;
        this.dependencies = [];
    }
}

function parseInput(input) {
    const monkeys = {};
    
    input.split('\n').forEach(line => {
        if (!line.trim()) return;
        
        const [name, jobStr] = line.split(': ');
        
        if (!isNaN(Number(jobStr))) {
            monkeys[name] = new Monkey(Number(jobStr));
        } else {
            const [left, op, right] = jobStr.split(' ');
            monkeys[name] = new Monkey({ left, op, right });
            monkeys[name].dependencies = [left, right];
        }
    });
    
    return monkeys;
}

function calculateValue(name, monkeys, humanValue = null) {
    // Special handling for human
    if (name === 'humn') {
        return humanValue;
    }
    
    const monkey = monkeys[name];
    
    // If it's a number monkey
    if (typeof monkey.job === 'number') {
        return monkey.job;
    }
    
    // Calculate dependent values
    const left = calculateValue(monkey.job.left, monkeys, humanValue);
    const right = calculateValue(monkey.job.right, monkeys, humanValue);
    
    // If either side is null, return null
    if (left === null || right === null) {
        return null;
    }
    
    // Perform calculation based on operator
    switch (monkey.job.op) {
        case '+': return left + right;
        case '-': return left - right;
        case '*': return left * right;
        case '/': return left / right;
    }
}

function findHumanNumber(monkeys) {
    const root = monkeys['root'];
    const leftMonkey = root.job.left;
    const rightMonkey = root.job.right;
    
    // Binary search to find the right human number
    let left = 0;
    let right = 10000000000000;
    
    while (left < right) {
        const mid = Math.floor((left + right) / 2);
        
        const leftValue = calculateValue(leftMonkey, {...monkeys}, mid);
        const rightValue = calculateValue(rightMonkey, {...monkeys}, mid);
        
        if (leftValue === rightValue) {
            return mid;
        }
        
        if (leftValue > rightValue) {
            left = mid + 1;
        } else {
            right = mid - 1;
        }
    }
    
    return left;
}

// Read input from file
const input = fs.readFileSync('input_level_21.txt', 'utf8');
const monkeys = parseInput(input);
const result = findHumanNumber(monkeys);
console.log('Number to yell:', result);