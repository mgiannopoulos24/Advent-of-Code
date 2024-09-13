import ballerina/io;

// Define constants for the problem
const int A_FACTOR = 16807;
const int B_FACTOR = 48271;
const int MODULO = 2147483647;
const int PAIR_COUNT = 5000000; // Now we compare 5 million pairs

// Function to generate the next value for a generator
function generateNext(int currentValue, int factor) returns int {
    return (currentValue * factor) % MODULO;
}

// Function to compare the lowest 16 bits of two numbers
function compareLowest16Bits(int valueA, int valueB) returns boolean {
    int mask = 0xFFFF; // Mask to extract the lowest 16 bits (65535 in decimal)
    return (valueA & mask) == (valueB & mask);
}

// Function to generate the next valid value for generator A or B
function getNextValidValue(int currentValue, int factor, int multiple) returns int {
    int nextValue = currentValue;
    while true {
        nextValue = generateNext(nextValue, factor);
        if (nextValue % multiple == 0) {
            return nextValue;
        }
    }
}

public function main() {
    // Initialize starting values for generators A and B
    int aValue = 289;  // Change this to the actual starting value for A
    int bValue = 629; // Change this to the actual starting value for B

    int matchCount = 0;
    int pairCount = 0;

    // Loop until we have 5 million valid pairs
    while pairCount < PAIR_COUNT {
        // Generate next valid values for A and B that meet the criteria
        aValue = getNextValidValue(aValue, A_FACTOR, 4); // Generator A needs to be divisible by 4
        bValue = getNextValidValue(bValue, B_FACTOR, 8); // Generator B needs to be divisible by 8

        // Compare the lowest 16 bits of both values
        if (compareLowest16Bits(aValue, bValue)) {
            matchCount += 1;
        }

        pairCount += 1;
    }

    // Output the total number of matches
    io:println("Total matches: ", matchCount);
}
