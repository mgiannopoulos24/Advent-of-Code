const fs = require('fs');

function parseMonkeyJobs(input) {
    const monkeyJobs = new Map();
    
    // Parse input lines into a map of monkey jobs
    input.split('\n').forEach(line => {
        if (line.trim() === '') return;
        
        const [monkey, job] = line.split(': ');
        
        // Check if job is a simple number
        if (!isNaN(Number(job))) {
            monkeyJobs.set(monkey, { 
                type: 'number', 
                value: Number(job) 
            });
        } else {
            // Parse operation jobs
            const [left, operator, right] = job.split(' ');
            monkeyJobs.set(monkey, { 
                type: 'operation', 
                left, 
                operator, 
                right 
            });
        }
    });
    
    return monkeyJobs;
}

function resolveMonkeyValue(monkey, monkeyJobs, resolvedValues = new Map()) {
    // If value already resolved, return it
    if (resolvedValues.has(monkey)) {
        return resolvedValues.get(monkey);
    }
    
    const job = monkeyJobs.get(monkey);
    
    // If it's a number job, just return the value
    if (job.type === 'number') {
        resolvedValues.set(monkey, job.value);
        return job.value;
    }
    
    // Resolve operation jobs
    const leftValue = resolveMonkeyValue(job.left, monkeyJobs, resolvedValues);
    const rightValue = resolveMonkeyValue(job.right, monkeyJobs, resolvedValues);
    
    let result;
    switch (job.operator) {
        case '+':
            result = leftValue + rightValue;
            break;
        case '-':
            result = leftValue - rightValue;
            break;
        case '*':
            result = leftValue * rightValue;
            break;
        case '/':
            result = leftValue / rightValue;
            break;
    }
    
    resolvedValues.set(monkey, result);
    return result;
}

function solveMonkeyMath(input) {
    const monkeyJobs = parseMonkeyJobs(input);
    return resolveMonkeyValue('root', monkeyJobs);
}

// Read input from file
const input = fs.readFileSync('input_level_21.txt', 'utf8');
const result = solveMonkeyMath(input);
console.log('Root monkey yells:', result);