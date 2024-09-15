import ballerina/io;
import ballerina/regex;

// Structure to represent a component
type Component record {
    int port1;
    int port2;
};

// Structure to represent the result of a bridge
type BridgeResult record {
    int length;
    int strength;
};

// Function to build all possible bridges and find the longest/strongest one
function buildBridge(int currentPort, Component[] availableComponents, Component[] currentBridge) returns BridgeResult {
    int currentLength = currentBridge.length();
    int currentStrength = calculateStrength(currentBridge);

    BridgeResult maxResult = {length: currentLength, strength: currentStrength};

    foreach Component comp in availableComponents {
        // Check if the component can connect to the currentPort
        if (comp.port1 == currentPort || comp.port2 == currentPort) {
            // Create a new bridge by adding the current component
            Component[] newBridge = currentBridge.clone();
            newBridge.push(comp);

            // Remove this component from the available ones for the next recursive call
            Component[] remainingComponents = [];
            foreach var c in availableComponents {
                if (c != comp) {
                    remainingComponents.push(c);
                }
            }

            // Determine the next port to connect
            int nextPort = (comp.port1 == currentPort) ? comp.port2 : comp.port1;

            // Recurse with the new bridge
            BridgeResult result = buildBridge(nextPort, remainingComponents, newBridge);

            // Check if the new bridge is longer, or if it is the same length but stronger
            if (result.length > maxResult.length || (result.length == maxResult.length && result.strength > maxResult.strength)) {
                maxResult = result;
            }
        }
    }

    return maxResult;
}

// Function to calculate the strength of the current bridge
function calculateStrength(Component[] bridge) returns int {
    int strength = 0;
    foreach Component comp in bridge {
        strength += comp.port1 + comp.port2;
    }
    return strength;
}

public function main() returns error? {
    // Read input from the file
    string input = check io:fileReadString("input_level_24.txt");

    // Split input into lines and parse components
    string[] lines = regex:split(input, "\\r?\\n");
    Component[] components = [];

    foreach var line in lines {
        if line.trim() != "" {
            string[] ports = regex:split(line, "/");
            int port1 = check 'int:fromString(ports[0]);
            int port2 = check 'int:fromString(ports[1]);
            components.push({port1: port1, port2: port2});
        }
    }

    // Start building the bridge from port 0
    BridgeResult longestBridge = buildBridge(0, components, []);

    // Output the length and strength of the longest bridge
    io:println("The length of the longest bridge is: ", longestBridge.length);
    io:println("The strength of the longest bridge is: ", longestBridge.strength);
}
