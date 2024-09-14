import ballerina/io;
import ballerina/regex;

// Define a Particle record to store position, velocity, and acceleration
type Particle record {
    int[] p;  // Position
    int[] v;  // Velocity
    int[] a;  // Acceleration
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

    // Correct loop for simulating the particle updates over the ticks
    foreach int _ in 0...ticks {
        // Update each particle's velocity and position
        foreach Particle particle in particles {
            updateParticle(particle);
        }
    }

    // Find the particle closest to the origin (by Manhattan distance)
    int closestIndex = -1;
    int minDistance = 2147483647; // Start with a large number (max int)

    foreach int i in 0..<particles.length() {  // Use correct length-based boundaries
        int distance = manhattanDistance(particles[i].p);
        if (distance < minDistance) {
            minDistance = distance;
            closestIndex = i;
        }
    }

    // Output the index of the particle that stays closest to the origin
    io:println("Particle closest to the origin: ", closestIndex);
}
