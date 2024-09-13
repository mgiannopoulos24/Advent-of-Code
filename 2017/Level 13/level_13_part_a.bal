import ballerina/io;
import ballerina/regex;

public function main() {
    // Read input from the file and build the firewall layers with their ranges
    map<int> firewall = readInputAsFirewall("input_level_13.txt");

    // Calculate the severity of the trip
    int severity = calculateSeverity(firewall);

    // Output the severity of the trip
    io:println("Severity of the whole trip: ", severity);
}

// Function to calculate the severity of the trip
function calculateSeverity(map<int> firewall) returns int {
    int severity = 0;

    // Loop through each depth in the firewall
    foreach var depthStr in firewall.keys() {
        int depth = checkpanic int:fromString(depthStr);
        
        // Safely get the range for the current depth using nil-coalescing
        int range = firewall[depth.toString()] ?: 0;  // Fallback to 0 if range is null

        // Calculate the scanner's position at the time the packet reaches this depth
        int cycleLength = 2 * (range - 1);
        int scannerPosition = depth % cycleLength;

        // If the scanner is at position 0, the packet is caught
        if (scannerPosition == 0 && range > 0) {
            severity += depth * range;
        }
    }

    return severity;
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
