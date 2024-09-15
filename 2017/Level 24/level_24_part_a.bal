import ballerina/io;
import ballerina/regex;

// Structure to represent a component
type Component record {
    int port1;
    int port2;
};

// Function to build all possible bridges and calculate the maximum strength
function buildBridge(int currentPort, Component[] availableComponents, Component[] currentBridge) returns int {
    int maxStrength = calculateStrength(currentBridge);

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
            int bridgeStrength = buildBridge(nextPort, remainingComponents, newBridge);
            if (bridgeStrength > maxStrength) {
                maxStrength = bridgeStrength;
            }
        }
    }

    return maxStrength;
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
    int maxBridgeStrength = buildBridge(0, components, []);

    // Output the maximum bridge strength
    io:println("The strength of the strongest bridge is: ", maxBridgeStrength);
}
