import ballerina/io;
import ballerina/regex;

function loadInitialConfiguration(string filename) returns int[]|error {
    // Load the initial configuration from the file
    string fileContent = check io:fileReadString(filename);
    string[] lines = regex:split(fileContent, "\n");
    string[] numbers = regex:split(lines[0], "\\s+");
    int[] initialState = [];
    foreach var numStr in numbers {
        int num = check 'int:fromString(numStr);
        initialState.push(num);
    }
    return initialState;
}

function reallocateBlocks(int[] banks) returns int[] {
    // Perform the block redistribution and return the new state
    int maxBlocks = 0;
    int index = 0;
    foreach var i in 0 ..< banks.length() {
        if (banks[i] > maxBlocks) {
            maxBlocks = banks[i];
            index = i;
        }
    }
    
    // Remove all blocks from that bank
    banks[index] = 0;
    
    // Distribute the blocks
    int numBanks = banks.length();
    foreach var _ in 0 ..< maxBlocks {
        index = (index + 1) % numBanks;
        banks[index] += 1;
    }
    
    return banks;
}

function countCyclesUntilRevisited(int[] initialState) returns int {
    // Count the number of redistribution cycles until a configuration is revisited and then the cycle length
    map<int> seenConfigurations = {};
    int[] banks = initialState.clone();
    int cycles = 0;
    
    while (!seenConfigurations.hasKey(banks.toString())) {
        seenConfigurations[banks.toString()] = cycles;
        banks = reallocateBlocks(banks);
        cycles += 1;
    }

    int? firstSeenAtOpt = seenConfigurations[banks.toString()];
    if (firstSeenAtOpt is int) {
        int firstSeenAt = firstSeenAtOpt;
        int loopSize = cycles - firstSeenAt;
        return loopSize;
    } else {
        // Handle the case where the value is null
        return -1; 
    }
}

public function main() returns error? {
    // Load the initial configuration
    int[] initialState = check loadInitialConfiguration("input_level_6.txt");
    
    // Calculate the size of the loop
    int loopSize = countCyclesUntilRevisited(initialState);
    
    // Output the result
    io:println("Cycles needed: ", loopSize);
}