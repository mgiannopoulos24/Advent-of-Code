import ballerina/io;

// Define constants for the problem
const int A_FACTOR = 16807;
const int B_FACTOR = 48271;
const int MODULO = 2147483647;
const int PAIR_COUNT = 40000000;

// Function to generate the next value for a generator
function generateNext(int currentValue, int factor) returns int {
    return (currentValue * factor) % MODULO;
}

// Function to compare the lowest 16 bits of two numbers
function compareLowest16Bits(int valueA, int valueB) returns boolean {
    int mask = 0xFFFF; // Mask to extract the lowest 16 bits (65535 in decimal)
    return (valueA & mask) == (valueB & mask);
}

public function main() {
    // Initialize starting values for generators A and B
    int aValue = 289;  // Change this to the actual starting value for A
    int bValue = 629; // Change this to the actual starting value for B

    int matchCount = 0;

    // Loop to generate values for 40 million pairs
    foreach int i in 0..<(PAIR_COUNT) {
        // Generate next values for A and B
        aValue = generateNext(aValue, A_FACTOR);
        bValue = generateNext(bValue, B_FACTOR);

        // Compare the lowest 16 bits of both values
        if (compareLowest16Bits(aValue, bValue)) {
            matchCount += 1;
        }
    }

    // Output the total number of matches
    io:println("Total matches: ", matchCount);
}
