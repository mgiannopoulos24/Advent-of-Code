import ballerina/io;
import ballerina/regex;

function loadInitialConfiguration(string filename) returns int[]|error {
    // Read the file content as a string
    string content = check io:fileReadString(filename);

    // Split the string by spaces to get individual block counts
    string[] parts = regex:split(content, "\\s+");
    int[] banks = [];
    foreach string part in parts {
        int value = check 'int:fromString(part);
        banks.push(value);
    }
    return banks;
}

function reallocateBlocks(int[] banks) returns int[] {
    // Find the bank with the most blocks
    int maxBlocks = banks[0];
    int index = 0;
    foreach int i in 0..<(banks.length()) {
        if banks[i] > maxBlocks {
            maxBlocks = banks[i];
            index = i;
        }
    }

    // Remove all blocks from that bank
    banks[index] = 0;

    // Distribute the blocks
    int numBanks = banks.length();
    foreach int _ in 0..<maxBlocks {
        index = (index + 1) % numBanks;
        banks[index] += 1;
    }

    return banks;
}

function countRedistributionCycles(int[] initialState) returns int {
    // Track seen configurations and count cycles
    map<json> seenConfigurations = {};
    int[] banks = initialState.clone();
    int cycles = 0;

    while (!seenConfigurations.hasKey(banks.toString())) {  // Correctly use hasKey() to check for key existence
        seenConfigurations[banks.toString()] = cycles;
        banks = reallocateBlocks(banks);
        cycles += 1;
    }

    return cycles;
}

public function main() {
    // Load the initial configuration
    int[]|error initialState = loadInitialConfiguration("input_level_6.txt");

    if initialState is int[] {
        // Calculate the number of redistribution cycles
        int cycles = countRedistributionCycles(initialState);
        // Output the result
        io:println("Cycles needed: ", cycles);
    } else {
        io:println("Error loading initial configuration: ", initialState.message());
    }
}
