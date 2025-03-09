const fs = require('fs');

function simulateTetrisCycles(jetPattern, targetRocks) {
  // Define the five rock shapes (using coordinates)
  const rocks = [
    // Horizontal line
    [[0, 0], [1, 0], [2, 0], [3, 0]],
    // Plus shape
    [[1, 0], [0, 1], [1, 1], [2, 1], [1, 2]],
    // Reverse L shape
    [[0, 0], [1, 0], [2, 0], [2, 1], [2, 2]],
    // Vertical line
    [[0, 0], [0, 1], [0, 2], [0, 3]],
    // Square
    [[0, 0], [1, 0], [0, 1], [1, 1]]
  ];

  // Initialize variables
  let jetIndex = 0;
  let rockIndex = 0;
  let highestPoint = -1;
  const tower = new Set();
  
  // State tracking for cycle detection
  const states = new Map();
  let cycleFound = false;
  let rocksSimulated = 0;
  let heightAdded = 0;

  while (rocksSimulated < targetRocks) {
    // Check for cycles if we haven't found one yet
    if (!cycleFound) {
      // State is defined by current rock index, jet index, and top configuration
      const topConfig = getTopConfiguration(tower, highestPoint);
      const state = `${rockIndex},${jetIndex},${topConfig}`;
      
      if (states.has(state)) {
        // We found a cycle
        const [prevRocks, prevHeight] = states.get(state);
        const cycleRocks = rocksSimulated - prevRocks;
        const cycleHeight = highestPoint - prevHeight;
        
        // Calculate how many full cycles we can skip
        const remainingRocks = targetRocks - rocksSimulated;
        const fullCycles = Math.floor(remainingRocks / cycleRocks);
        
        // Skip ahead
        heightAdded = fullCycles * cycleHeight;
        rocksSimulated += fullCycles * cycleRocks;
        cycleFound = true;
      } else {
        // Record current state
        states.set(state, [rocksSimulated, highestPoint]);
      }
    }

    // Get the current rock shape
    const rockShape = rocks[rockIndex];
    rockIndex = (rockIndex + 1) % rocks.length;

    // Set initial position (x, y) of the rock
    let rockPos = { x: 2, y: highestPoint + 4 };

    // Keep moving the rock until it stops
    let rockStopped = false;
    while (!rockStopped) {
      // Get current jet and move index
      const jet = jetPattern[jetIndex];
      jetIndex = (jetIndex + 1) % jetPattern.length;

      // Try to move rock horizontally based on jet
      const horizontalMove = jet === '>' ? 1 : -1;
      const proposedHorizontalPos = { x: rockPos.x + horizontalMove, y: rockPos.y };

      // Check if horizontal move is valid
      if (isValidPosition(rockShape, proposedHorizontalPos, tower)) {
        rockPos = proposedHorizontalPos;
      }

      // Try to move rock down
      const proposedVerticalPos = { x: rockPos.x, y: rockPos.y - 1 };
      
      // Check if vertical move is valid
      if (isValidPosition(rockShape, proposedVerticalPos, tower)) {
        rockPos = proposedVerticalPos;
      } else {
        // Rock has stopped, add its positions to the tower
        rockStopped = true;
        for (const [dx, dy] of rockShape) {
          const x = rockPos.x + dx;
          const y = rockPos.y + dy;
          tower.add(`${x},${y}`);
          highestPoint = Math.max(highestPoint, y);
        }
      }
    }
    
    rocksSimulated++;
  }

  // Return the height of the tower
  return highestPoint + 1 + heightAdded; // +1 because we're zero-indexed
}

// Get a representation of the top of the tower for cycle detection
function getTopConfiguration(tower, highestPoint) {
  // Store relative heights of the top few rows
  const topConfig = [];
  
  // For each column
  for (let x = 0; x < 7; x++) {
    // Find how far down from the highest point we need to go to find a rock
    let relativeHeight = 0;
    while (relativeHeight <= 30 && !tower.has(`${x},${highestPoint - relativeHeight}`)) {
      relativeHeight++;
    }
    topConfig.push(relativeHeight);
  }
  
  return topConfig.join(',');
}

function isValidPosition(rockShape, pos, tower) {
  for (const [dx, dy] of rockShape) {
    const x = pos.x + dx;
    const y = pos.y + dy;
    
    // Check if out of bounds on left, right or bottom
    if (x < 0 || x >= 7 || y < 0) {
      return false;
    }
    
    // Check if colliding with a stopped rock
    if (tower.has(`${x},${y}`)) {
      return false;
    }
  }
  
  return true;
}

// Read input file
function main() {
  try {
    // Read the input
    const jetPattern = fs.readFileSync('input_level_17.txt', 'utf8').trim();
    
    // For part 1 (optional)
    const height1 = simulateTetrisCycles(jetPattern, 2022);
    console.log(`Part 1: The tower of rocks will be ${height1} units tall after 2022 rocks.`);
    
    // For part 2
    const height2 = simulateTetrisCycles(jetPattern, 1000000000000);
    console.log(`Part 2: The tower of rocks will be ${height2} units tall after 1000000000000 rocks.`);
  } catch (err) {
    console.error('Error:', err);
  }
}

main();