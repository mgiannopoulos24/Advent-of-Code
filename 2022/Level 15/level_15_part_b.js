const fs = require('fs');

// Function to calculate Manhattan distance
function manhattanDistance(x1, y1, x2, y2) {
    return Math.abs(x1 - x2) + Math.abs(y1 - y2);
}

// Function to parse the input file
function parseInput(filePath) {
    const sensors = [];
    const beacons = new Set();
    const data = fs.readFileSync(filePath, 'utf8').split('\n');
    for (const line of data) {
        const match = line.match(/-?\d+/g);
        if (match) {
            const [sx, sy, bx, by] = match.map(Number);
            sensors.push([sx, sy, bx, by]);
            beacons.add(`${bx},${by}`);
        }
    }
    return { sensors, beacons };
}

// Function to find the distress beacon
function findDistressBeacon(sensors, maxCoord) {
    for (let y = 0; y <= maxCoord; y++) {
        const excludedRanges = [];
        for (const [sx, sy, bx, by] of sensors) {
            const dist = manhattanDistance(sx, sy, bx, by);
            const verticalDist = Math.abs(sy - y);
            if (verticalDist <= dist) {
                const maxXOffset = dist - verticalDist;
                const x1 = sx - maxXOffset;
                const x2 = sx + maxXOffset;
                excludedRanges.push([x1, x2]);
            }
        }

        // Sort and merge overlapping ranges
        excludedRanges.sort((a, b) => a[0] - b[0]);
        const mergedRanges = [];
        let currentRange = excludedRanges[0];
        for (let i = 1; i < excludedRanges.length; i++) {
            const nextRange = excludedRanges[i];
            if (nextRange[0] <= currentRange[1] + 1) {
                currentRange = [
                    Math.min(currentRange[0], nextRange[0]),
                    Math.max(currentRange[1], nextRange[1]),
                ];
            } else {
                mergedRanges.push(currentRange);
                currentRange = nextRange;
            }
        }
        mergedRanges.push(currentRange);

        // Check for gaps in the merged ranges
        for (let i = 0; i < mergedRanges.length - 1; i++) {
            if (mergedRanges[i][1] + 1 < mergedRanges[i + 1][0]) {
                const x = mergedRanges[i][1] + 1;
                if (x >= 0 && x <= maxCoord) {
                    return [x, y];
                }
            }
        }
    }
    return null;
}

// Main function
function main() {
    const filePath = 'input_level_15.txt'; // Change this to your actual input file
    const maxCoord = 4000000;
    const { sensors } = parseInput(filePath);
    const result = findDistressBeacon(sensors, maxCoord);

    if (result) {
        const [x, y] = result;
        const tuningFrequency = x * 4000000 + y;
        console.log("Tuning frequency:", tuningFrequency);
    } else {
        console.log("No distress beacon found.");
    }
}

// Run the program
main();