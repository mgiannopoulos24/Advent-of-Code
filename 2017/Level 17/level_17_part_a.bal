import ballerina/io;

public function main() {
    int steps = 377; // Puzzle input
    int maxInsertions = 2017; 
    int[] buffer = [0]; 
    int currentPosition = 0; 
    
    // Loop to insert values from 1 to maxInsertions (2017)
    foreach int i in 1 ... maxInsertions {
        currentPosition = (currentPosition + steps) % buffer.length() + 1;
        
        buffer = insertValue(buffer, currentPosition, i);
    }
    
    int? positionOf2017Opt = buffer.indexOf(2017);
    if positionOf2017Opt is int {
        int positionOf2017 = positionOf2017Opt;
        int valueAfter2017 = buffer[(positionOf2017 + 1) % buffer.length()];
        io:println("The value after 2017 is: ", valueAfter2017);
    } else {
        io:println("Error: Could not find 2017 in the buffer.");
    }
}

// Function to insert a value at a specific position in the buffer
function insertValue(int[] buffer, int position, int value) returns int[] {
    int[] newBuffer = [];
    
    foreach int i in 0 ..< position {
        newBuffer.push(buffer[i]);
    }
    
    newBuffer.push(value);
    
    foreach int i in position ..< buffer.length() {
        newBuffer.push(buffer[i]);
    }
    
    return newBuffer;
}
