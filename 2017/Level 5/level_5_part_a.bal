import ballerina/io;
import ballerina/regex;

public function main() returns error? {
    // Read the list of jump offsets from a text file
    string filePath = "input_level_5.txt";
    string lines = check io:fileReadString(filePath);

    // Split the lines by newline characters
    string[] lineArray = regex:split(lines,"\n");

    // Convert lines to integers
    int[] offsets = [];
    foreach string line in lineArray {
        // Trim whitespace and convert to integer
        string trimmedLine = line.trim();
        if (trimmedLine.length() > 0) {
            offsets.push(check int:fromString(trimmedLine));
        }
    }

    // Initialize variables
    int steps = 0;
    int currentPosition = 0;

    // Process jumps
    while (currentPosition >= 0 && currentPosition < offsets.length()) {
        int jump = offsets[currentPosition];
        // Modify the instruction
        offsets[currentPosition] = jump + 1;
        // Move to the next instruction
        currentPosition += jump;
        // Increment the step counter
        steps += 1;
    }

    // Print the number of steps to reach the exit
    io:println("Steps to reach the exit: ",steps);
}
