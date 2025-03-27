const fs = require('fs');

function parseBlueprints(input) {
    return input.trim().split('\n').map(line => {
        const numbers = line.match(/\d+/g).map(Number);
        return {
            id: numbers[0],
            oreRobotCost: { ore: numbers[1] },
            clayRobotCost: { ore: numbers[2] },
            obsidianRobotCost: { ore: numbers[3], clay: numbers[4] },
            geodeRobotCost: { ore: numbers[5], obsidian: numbers[6] }
        };
    });
}

// Simulate blueprint to maximize geodes
function simulateBlueprint(blueprint, timeLimit) {
    // State represents: [ore, clay, obsidian, geodes, oreRobots, clayRobots, obsidianRobots, geodeRobots]
    const initialState = [0, 0, 0, 0, 1, 0, 0, 0];
    
    const maxOreCost = Math.max(
        blueprint.oreRobotCost.ore, 
        blueprint.clayRobotCost.ore, 
        blueprint.obsidianRobotCost.ore, 
        blueprint.geodeRobotCost.ore
    );

    const seen = new Set();
    const queue = [[...initialState, 0]]; // Add time to the state
    let maxGeodes = 0;

    while (queue.length) {
        let [ore, clay, obsidian, geodes, 
             oreRobots, clayRobots, obsidianRobots, geodeRobots, time] = queue.pop();

        // Prune if time is up
        if (time === timeLimit) {
            maxGeodes = Math.max(maxGeodes, geodes);
            continue;
        }

        // Prune impossible paths
        if (time > timeLimit) continue;

        // Prune based on maximum possible geodes
        const remainingTime = timeLimit - time;
        const potentialGeodes = geodes + geodeRobots * remainingTime + 
            (remainingTime * (remainingTime - 1)) / 2;
        if (potentialGeodes <= maxGeodes) continue;

        // State encoding for memoization
        const stateKey = `${ore},${clay},${obsidian},${geodes},${oreRobots},${clayRobots},${obsidianRobots},${geodeRobots},${time}`;
        if (seen.has(stateKey)) continue;
        seen.add(stateKey);

        // Collect resources
        const newOre = ore + oreRobots;
        const newClay = clay + clayRobots;
        const newObsidian = obsidian + obsidianRobots;
        const newGeodes = geodes + geodeRobots;

        // Try building each type of robot
        const options = [];

        // Build geode robot if possible
        if (ore >= blueprint.geodeRobotCost.ore && 
            obsidian >= blueprint.geodeRobotCost.obsidian) {
            options.push([
                newOre - blueprint.geodeRobotCost.ore, 
                newClay, 
                newObsidian - blueprint.geodeRobotCost.obsidian, 
                newGeodes, 
                oreRobots, clayRobots, obsidianRobots, 
                geodeRobots + 1, 
                time + 1
            ]);
        }

        // Build obsidian robot if possible and useful
        if (ore >= blueprint.obsidianRobotCost.ore && 
            clay >= blueprint.obsidianRobotCost.clay &&
            obsidianRobots < blueprint.geodeRobotCost.obsidian) {
            options.push([
                newOre - blueprint.obsidianRobotCost.ore, 
                newClay - blueprint.obsidianRobotCost.clay, 
                newObsidian, 
                newGeodes, 
                oreRobots, clayRobots, 
                obsidianRobots + 1, 
                geodeRobots, 
                time + 1
            ]);
        }

        // Build clay robot if possible and useful
        if (ore >= blueprint.clayRobotCost.ore && 
            clayRobots < blueprint.obsidianRobotCost.clay) {
            options.push([
                newOre - blueprint.clayRobotCost.ore, 
                newClay, 
                newObsidian, 
                newGeodes, 
                oreRobots, clayRobots + 1, 
                obsidianRobots, 
                geodeRobots, 
                time + 1
            ]);
        }

        // Build ore robot if possible and useful
        if (ore >= blueprint.oreRobotCost.ore && 
            oreRobots < maxOreCost) {
            options.push([
                newOre - blueprint.oreRobotCost.ore, 
                newClay, 
                newObsidian, 
                newGeodes, 
                oreRobots + 1, clayRobots, 
                obsidianRobots, 
                geodeRobots, 
                time + 1
            ]);
        }

        // Wait without building
        options.push([
            newOre, newClay, newObsidian, newGeodes, 
            oreRobots, clayRobots, obsidianRobots, geodeRobots, 
            time + 1
        ]);

        queue.push(...options);
    }

    return maxGeodes;
}

// Main function to solve the puzzle
function solveBlueprints(input, timeLimit = 24) {
    const blueprints = parseBlueprints(input);
    
    const qualityLevels = blueprints.map(blueprint => {
        const maxGeodes = simulateBlueprint(blueprint, timeLimit);
        return blueprint.id * maxGeodes;
    });

    return qualityLevels.reduce((a, b) => a + b, 0);
}

fs.readFile('input_level_19.txt', 'utf8', (err, data) => {
    if (err) {
        console.error('Error reading input file:', err);
        return;
    }
    
    const result = solveBlueprints(data);
    console.log('Total Quality Level:', result);
});