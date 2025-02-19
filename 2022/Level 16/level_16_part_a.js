const fs = require('fs');

function parseInput(inputData) {
    const valves = {};
    for (const line of inputData) {
        const matches = line.match(/[A-Z]{2}|\d+/g);
        const valve = matches[0];
        const flowRate = parseInt(matches[1]);
        const tunnels = matches.slice(2);
        valves[valve] = { flowRate, tunnels };
    }
    return valves;
}

function bfs(valves, start) {
    const distances = { [start]: 0 };
    const queue = [start];
    while (queue.length > 0) {
        const current = queue.shift();
        for (const neighbor of valves[current].tunnels) {
            if (!(neighbor in distances)) {
                distances[neighbor] = distances[current] + 1;
                queue.push(neighbor);
            }
        }
    }
    return distances;
}

function findMaxPressure(valves, currentValve, timeLeft, opened, cache) {
    if (timeLeft <= 0) {
        return 0;
    }

    const key = `${currentValve},${timeLeft},${[...opened].sort().join(',')}`;
    if (cache[key] !== undefined) {
        return cache[key];
    }

    let maxPressure = 0;

    // Option 1: Open the current valve if it's not already open and has a flow rate > 0
    if (!opened.has(currentValve) && valves[currentValve].flowRate > 0) {
        const newOpened = new Set(opened);
        newOpened.add(currentValve);
        const pressureReleased = valves[currentValve].flowRate * (timeLeft - 1);
        maxPressure = Math.max(maxPressure, pressureReleased + findMaxPressure(valves, currentValve, timeLeft - 1, newOpened, cache));
    }

    // Option 2: Move to a neighboring valve
    for (const neighbor of valves[currentValve].tunnels) {
        maxPressure = Math.max(maxPressure, findMaxPressure(valves, neighbor, timeLeft - 1, opened, cache));
    }

    cache[key] = maxPressure;
    return maxPressure;
}

function main() {
    const inputData = fs.readFileSync('input_level_16.txt', 'utf8').split('\n');
    const valves = parseInput(inputData);
    const distances = {};
    for (const valve in valves) {
        distances[valve] = bfs(valves, valve);
    }

    // Only consider valves with a flow rate > 0
    const relevantValves = new Set(Object.keys(valves).filter(valve => valves[valve].flowRate > 0));

    // Start at valve 'AA' with 30 minutes left
    const maxPressure = findMaxPressure(valves, 'AA', 30, new Set(), {});
    console.log(`Maximum pressure released: ${maxPressure}`);
}

main();