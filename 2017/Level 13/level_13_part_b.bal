import ballerina/io;
import ballerina/regex;

public function main() {
    // Read input from the file and build the firewall layers with their ranges
    map<int> firewall = readInputAsFirewall("input_level_13.txt");

    // Find the minimum delay that avoids being caught
    int delay = findMinimumSafeDelay(firewall);

    // Output the result
    io:println("The fewest number of picoseconds to delay to pass safely: ", delay);
}

// Function to find the minimum delay to avoid getting caught
function findMinimumSafeDelay(map<int> firewall) returns int {
    int delay = 0;

    // Keep increasing delay until we find a safe one
    while true {
        if (isSafeTraversal(firewall, delay)) {
            return delay;
        }
        delay += 1;
    }
}

// Function to check if the packet can traverse safely without getting caught
function isSafeTraversal(map<int> firewall, int delay) returns boolean {
    // Loop through each depth in the firewall
    foreach var depthStr in firewall.keys() {
        int depth = checkpanic int:fromString(depthStr);
        int range = firewall[depth.toString()] ?: 0;

        // Calculate the scanner's position when the packet reaches this depth
        int cycleLength = 2 * (range - 1);
        if (range > 0 && ((delay + depth) % cycleLength == 0)) {
            // If scanner is at position 0 when packet arrives, it's caught
            return false;
        }
    }
    return true; // Packet can pass safely without getting caught
}

// Function to read the input from a file and build the firewall map
function readInputAsFirewall(string filePath) returns map<int> {
    string content = checkpanic io:fileReadString(filePath);
    string[] lines = regex:split(content, "\n");

    map<int> firewall = {};

    foreach string line in lines {
        if line.trim().length() > 0 {
            // Split the line into depth and range
            string[] parts = regex:split(line, ": ");
            int depth = checkpanic int:fromString(parts[0].trim());
            int range = checkpanic int:fromString(parts[1].trim());

            firewall[depth.toString()] = range;
        }
    }

    return firewall;
}
