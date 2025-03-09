const fs = require('fs');

function simulateTetris(jetPattern, numRocks) {
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
  let highestPoint = -1; // Tower starts empty
  const tower = new Set(); // Track filled positions with a set

  // Process rocks falling
  for (let i = 0; i < numRocks; i++) {
    // Get the current rock shape
    const rockShape = rocks[rockIndex];
    rockIndex = (rockIndex + 1) % rocks.length;

    // Set initial position (x, y) of the rock
    // x: 2 units from left wall
    // y: 3 units above highest rock or floor
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
  }

  // Return the height of the tower
  return highestPoint + 1; // +1 because we're zero-indexed
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
    
    // Simulate 2022 rocks falling
    const height = simulateTetris(jetPattern, 2022);
    
    console.log(`The tower of rocks will be ${height} units tall after 2022 rocks have stopped falling.`);
  } catch (err) {
    console.error('Error:', err);
  }
}

main();