import ballerina/io;
import ballerina/regex;

// Define a Particle record to store position, velocity, and acceleration
type Particle record {
    int[] p;  // Position
    int[] v;  // Velocity
    int[] a;  // Acceleration
    boolean active = true; 
};

// Custom abs function to calculate the absolute value of an integer
function abs(int value) returns int {
    return value < 0 ? -value : value;
}

// Helper function to convert a string array to an integer array with error handling
function stringArrayToIntArray(string str) returns int[]|error {
    string[] strArr = regex:split(str, ",");
    int[] intArr = [];
    foreach string s in strArr {
        if s != "" {
            int value = check int:fromString(s); // Convert each string to int, check for errors
            intArr.push(value);
        }
    }
    return intArr;
}

// Parse a line representing a particle in the format `p=<X,Y,Z>, v=<X,Y,Z>, a=<X,Y,Z>`
function parseParticle(string line) returns Particle|error {
    // Extract position, velocity, and acceleration manually by finding the correct substrings
    int? pStartOpt = line.indexOf("p=<");
    int? vStartOpt = line.indexOf("v=<");
    int? aStartOpt = line.indexOf("a=<");

    if pStartOpt is () || vStartOpt is () || aStartOpt is () {
        return error("Invalid format: missing p, v, or a components");
    }

    int pStart = pStartOpt + 3;
    int? pEndOpt = line.indexOf(">", pStart);
    int vStart = vStartOpt + 3;
    int? vEndOpt = line.indexOf(">", vStart);
    int aStart = aStartOpt + 3;
    int? aEndOpt = line.indexOf(">", aStart);

    if pEndOpt is () || vEndOpt is () || aEndOpt is () {
        return error("Invalid format: missing closing bracket for p, v, or a");
    }

    int pEnd = pEndOpt;
    int vEnd = vEndOpt;
    int aEnd = aEndOpt;

    string pStr = line.substring(pStart, pEnd);
    string vStr = line.substring(vStart, vEnd);
    string aStr = line.substring(aStart, aEnd);

    int[] p = check stringArrayToIntArray(pStr);
    int[] v = check stringArrayToIntArray(vStr);
    int[] a = check stringArrayToIntArray(aStr);

    return {p: p, v: v, a: a};
}

// Manhattan distance calculation for a given position (x, y, z)
function manhattanDistance(int[] pos) returns int {
    return abs(pos[0]) + abs(pos[1]) + abs(pos[2]);
}

// Update the particle's velocity and position based on acceleration
function updateParticle(Particle particle) {
    foreach int i in 0...2 {  // Ensure valid bounds for index access
        particle.v[i] += particle.a[i];  // Update velocity
        particle.p[i] += particle.v[i];  // Update position
    }
}

// Custom function to check if an array contains a given element
function contains(int[] arr, int element) returns boolean {
    foreach int i in arr {
        if i == element {
            return true;
        }
    }
    return false;
}

// Detect collisions and return the updated particle list without in-place modification
function detectAndRemoveCollisions(Particle[] particles) returns Particle[] {
    // Map to track the positions and the indices of particles at those positions
    map<int[]> positionMap = {};

    // Build the map with active particles
    foreach int i in 0..<particles.length() {
        if particles[i].active {
            string posKey = particles[i].p.toString();
            int[]? existingIndices = positionMap[posKey];
            if existingIndices is int[] {
                existingIndices.push(i);  // Unwrapped, safe to push to the array
            } else {
                positionMap[posKey] = [i];  // First particle at this position
            }
        }
    }

    // Collect the indices of particles to be marked inactive
    int[] indicesToMarkInactive = [];

    // Remove particles that collide (more than one particle in the same position)
    foreach var [posKey, particleIndices] in positionMap.entries() {
        if particleIndices.length() > 1 {  // More than one particle at this position
            // Collect all particles at this position to mark inactive
            foreach int index in particleIndices {
                if index < particles.length() && particles[index].active {
                    indicesToMarkInactive.push(index);
                }
            }
        }
    }

    // Create a new list for active particles after removing collisions
    Particle[] remainingParticles = [];

    foreach int i in 0..<particles.length() {
        if !contains(indicesToMarkInactive, i) {  // Use custom contains function here
            remainingParticles.push(particles[i]);
        }
    }

    return remainingParticles;  // Return the updated particles array
}

public function main() returns error? {
    // Read the particle data from the input file
    string input = check io:fileReadString("input_level_20.txt");

    // Split the input into lines
    string[] lines = regex:split(input, "\n");

    // Parse the particles
    Particle[] particles = [];
    foreach string line in lines {
        if line != "" {
            Particle particle = check parseParticle(line);
            particles.push(particle);
        }
    }

    int ticks = 1000; // Simulate a large number of ticks to determine long-term behavior

    // Simulate the particle updates and detect collisions
    foreach int _ in 0...ticks {
        // Safeguard: Iterate explicitly over the indices of particles to ensure safe access
        foreach int j in 0..<particles.length() {
            if particles[j].active {
                updateParticle(particles[j]);
            }
        }

        // Detect and flag colliding particles, then remove them after iteration
        particles = detectAndRemoveCollisions(particles);
    }

    // Count remaining particles that are still active
    int remainingParticles = 0;
    foreach Particle particle in particles {
        if particle.active {
            remainingParticles += 1;
        }
    }

    // Output the number of particles left after resolving collisions
    io:println("Particles left after collisions: ", remainingParticles);
}
