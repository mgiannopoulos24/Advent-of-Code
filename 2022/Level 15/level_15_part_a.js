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

// Function to find excluded positions
function findExcludedPositions(sensors, beacons, targetY) {
    const exclusionZones = new Set();
    for (const [sx, sy, bx, by] of sensors) {
        const dist = manhattanDistance(sx, sy, bx, by);
        const verticalDist = Math.abs(sy - targetY);
        if (verticalDist <= dist) {
            const maxXOffset = dist - verticalDist;
            for (let x = sx - maxXOffset; x <= sx + maxXOffset; x++) {
                exclusionZones.add(x);
            }
        }
    }

    // Remove positions where a beacon is already present in the row
    const beaconPositions = new Set();
    for (const beacon of beacons) {
        const [bx, by] = beacon.split(',').map(Number);
        if (by === targetY) {
            beaconPositions.add(bx);
        }
    }

    // Calculate the number of excluded positions
    let count = 0;
    for (const x of exclusionZones) {
        if (!beaconPositions.has(x)) {
            count++;
        }
    }
    return count;
}

// Main function
function main() {
    const filePath = 'input_level_15.txt'; // Change this to your actual input file
    const targetY = 2000000;
    const { sensors, beacons } = parseInput(filePath);
    const result = findExcludedPositions(sensors, beacons, targetY);
    console.log("Positions where a beacon cannot be present:", result);
}

// Run the program
main();