const fs = require('fs');

function calculateSurfaceArea(filename) {
  // Read the input file
  const input = fs.readFileSync(filename, 'utf8').trim();
  
  // Parse the coordinates
  const cubes = input.split('\n').map(line => {
    const [x, y, z] = line.split(',').map(Number);
    return { x, y, z };
  });
  
  // Create a set for quick lookup of cube positions
  const cubeSet = new Set();
  cubes.forEach(cube => {
    cubeSet.add(`${cube.x},${cube.y},${cube.z}`);
  });
  
  // Calculate the total surface area
  let totalSurfaceArea = 0;
  
  // Define the 6 possible adjacent positions (up, down, left, right, front, back)
  const directions = [
    { dx: 1, dy: 0, dz: 0 },
    { dx: -1, dy: 0, dz: 0 },
    { dx: 0, dy: 1, dz: 0 },
    { dx: 0, dy: -1, dz: 0 },
    { dx: 0, dy: 0, dz: 1 },
    { dx: 0, dy: 0, dz: -1 }
  ];
  
  // For each cube, check its 6 sides
  cubes.forEach(cube => {
    // Each cube starts with 6 exposed sides
    let exposedSides = 6;
    
    // Check each direction
    directions.forEach(dir => {
      const adjacentPos = `${cube.x + dir.dx},${cube.y + dir.dy},${cube.z + dir.dz}`;
      
      // If there's a cube in this adjacent position, reduce the exposed sides
      if (cubeSet.has(adjacentPos)) {
        exposedSides--;
      }
    });
    
    // Add this cube's exposed sides to the total
    totalSurfaceArea += exposedSides;
  });
  
  return totalSurfaceArea;
}

const surfaceArea = calculateSurfaceArea('input_level_18.txt');
console.log('Total surface area:', surfaceArea);

module.exports = calculateSurfaceArea;