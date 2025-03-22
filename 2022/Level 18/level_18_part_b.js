const fs = require('fs');

function calculateExteriorSurfaceArea(filename) {
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
  
  // Find the bounds of our grid (plus 1 in each direction to ensure we can "flood" around the edges)
  let minX = Infinity, maxX = -Infinity;
  let minY = Infinity, maxY = -Infinity;
  let minZ = Infinity, maxZ = -Infinity;
  
  cubes.forEach(cube => {
    minX = Math.min(minX, cube.x);
    maxX = Math.max(maxX, cube.x);
    minY = Math.min(minY, cube.y);
    maxY = Math.max(maxY, cube.y);
    minZ = Math.min(minZ, cube.z);
    maxZ = Math.max(maxZ, cube.z);
  });
  
  // Add some padding for the flood fill
  minX -= 1; maxX += 1;
  minY -= 1; maxY += 1;
  minZ -= 1; maxZ += 1;
  
  // Define the 6 possible adjacent positions (up, down, left, right, front, back)
  const directions = [
    { dx: 1, dy: 0, dz: 0 },
    { dx: -1, dy: 0, dz: 0 },
    { dx: 0, dy: 1, dz: 0 },
    { dx: 0, dy: -1, dz: 0 },
    { dx: 0, dy: 0, dz: 1 },
    { dx: 0, dy: 0, dz: -1 }
  ];
  
  // Perform a 3D flood fill from outside the lava droplet
  const visited = new Set(); // Keep track of visited air positions
  const queue = [{ x: minX, y: minY, z: minZ }]; // Start from a corner outside the droplet
  
  // Add the starting point to visited
  visited.add(`${minX},${minY},${minZ}`);
  
  // Surface area counter
  let exteriorSurfaceArea = 0;
  
  // Flood fill
  while (queue.length > 0) {
    const current = queue.shift();
    
    // Check each direction
    for (const dir of directions) {
      const nx = current.x + dir.dx;
      const ny = current.y + dir.dy;
      const nz = current.z + dir.dz;
      const nextPos = `${nx},${ny},${nz}`;
      
      // Skip if outside bounds
      if (nx < minX || nx > maxX || ny < minY || ny > maxY || nz < minZ || nz > maxZ) {
        continue;
      }
      
      // If we hit a cube, it's an exterior surface
      if (cubeSet.has(nextPos)) {
        exteriorSurfaceArea++;
        continue;
      }
      
      // If it's air and we haven't visited it yet, add to queue
      if (!visited.has(nextPos)) {
        visited.add(nextPos);
        queue.push({ x: nx, y: ny, z: nz });
      }
    }
  }
  
  return exteriorSurfaceArea;
}

const exteriorSurfaceArea = calculateExteriorSurfaceArea('input_level_18.txt');
console.log('Exterior surface area:', exteriorSurfaceArea);

module.exports = calculateExteriorSurfaceArea;