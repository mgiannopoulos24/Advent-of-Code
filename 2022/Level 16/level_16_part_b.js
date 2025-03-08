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

// Create optimized graph with only relevant valves and direct distances between them
function createOptimizedGraph(valves) {
  const relevantValves = Object.keys(valves)
    .filter(valve => valves[valve].flowRate > 0 || valve === 'AA')
    .sort();
  
  const optimizedGraph = {};
  
  for (const valve of relevantValves) {
    const distances = bfs(valves, valve);
    optimizedGraph[valve] = {
      flowRate: valves[valve].flowRate,
      tunnels: {}
    };
    
    for (const target of relevantValves) {
      if (target !== valve) {
        optimizedGraph[valve].tunnels[target] = distances[target];
      }
    }
  }
  
  return optimizedGraph;
}

// Generate all possible paths a single actor could take
function generatePaths(graph, timeLeft, current, opened, pressure, paths) {
  // Save the current state as a valid path
  const openedString = [...opened].sort().join(',');
  paths[openedString] = Math.max(paths[openedString] || 0, pressure);
  
  // Try opening each unopened valve
  for (const nextValve in graph) {
    if (nextValve !== 'AA' && !opened.has(nextValve)) {
      const timeToMove = graph[current].tunnels[nextValve];
      const timeToOpen = 1;
      const newTimeLeft = timeLeft - timeToMove - timeToOpen;
      
      // Check if we have enough time to open this valve
      if (newTimeLeft > 0) {
        const newOpened = new Set(opened);
        newOpened.add(nextValve);
        const additionalPressure = graph[nextValve].flowRate * newTimeLeft;
        
        generatePaths(
          graph, 
          newTimeLeft, 
          nextValve, 
          newOpened, 
          pressure + additionalPressure, 
          paths
        );
      }
    }
  }
  
  return paths;
}

function main() {
  const inputData = fs.readFileSync('input.txt', 'utf8').split('\n');
  const valves = parseInput(inputData);
  
  // Create optimized graph with only relevant valves
  const optimizedGraph = createOptimizedGraph(valves);
  
  // Time left is now 26 minutes (instead of 30)
  const timeLeft = 26;
  
  // Generate all possible paths for a single actor
  const paths = generatePaths(optimizedGraph, timeLeft, 'AA', new Set(), 0, {});
  
  // Find the best combination of two non-overlapping paths
  let maxPressure = 0;
  const pathEntries = Object.entries(paths);
  
  for (let i = 0; i < pathEntries.length; i++) {
    const [valvesYou, pressureYou] = pathEntries[i];
    const valvesYouSet = new Set(valvesYou.split(',').filter(v => v !== ''));
    
    for (let j = i + 1; j < pathEntries.length; j++) {
      const [valvesElephant, pressureElephant] = pathEntries[j];
      const valvesElephantSet = new Set(valvesElephant.split(',').filter(v => v !== ''));
      
      // Check if paths are non-overlapping (no common valves opened)
      const hasOverlap = [...valvesYouSet].some(valve => valvesElephantSet.has(valve));
      
      if (!hasOverlap) {
        const totalPressure = pressureYou + pressureElephant;
        maxPressure = Math.max(maxPressure, totalPressure);
      }
    }
  }
  
  console.log(`Maximum pressure released with elephant helping: ${maxPressure}`);
}

main();