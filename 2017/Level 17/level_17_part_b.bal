import ballerina/io;

public function main() {
    int steps = 377; // Puzzle input
    int maxInsertions = 50000000; // 50 million insertions
    int currentPosition = 0; // Current position in the buffer
    int valueAfterZero = -1; // To track the value after 0
    int bufferSize = 1; // Initially the buffer contains only one element (0)
    
    // Simulate the process for 50 million insertions
    foreach int i in 1 ... maxInsertions {
        // Calculate the next position using modulo arithmetic
        currentPosition = (currentPosition + steps) % bufferSize + 1;

        // If the current position is 1, the inserted value is right after 0
        if (currentPosition == 1) {
            valueAfterZero = i;
        }

        // Increase the buffer size after each insertion
        bufferSize += 1;
    }
    
    // Output the value after 0
    io:println("The value after 0 is: ", valueAfterZero);
}
