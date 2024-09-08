import ballerina/io;
import ballerina/regex;

function knotHash(string inputFile) returns int|error {
    // Read the input from the text file
    string content = check io:fileReadString(inputFile);
    int[] lengths = [];

    // Convert the comma-separated string into an array of integers
    foreach var part in regex:split(content,",") {
        int value = check 'int:fromString(part.trim());
        lengths.push(value);
    }

    // Initialize the list of numbers and other variables
    int[] numbers = [];
    foreach int i in 0 ... 255 {
        numbers.push(i);
    }

    int currentPosition = 0;
    int skipSize = 0;
    int n = numbers.length();

    foreach int length in lengths {
        if length > n {
            continue;
        }

        // Reverse the section of the list
        int[] toReverse = [];
        foreach int i in 0..<length {
            toReverse.push(numbers[(currentPosition + i) % n]);
        }
        // Explicitly reassign after reversing
        toReverse = toReverse.reverse();

        // Place the reversed section back into the list
        foreach int i in 0..<length {
            numbers[(currentPosition + i) % n] = toReverse[i];
        }

        // Move the current position forward
        currentPosition = (currentPosition + length + skipSize) % n;

        // Increase the skip size
        skipSize += 1;
    }

    // Multiply the first two numbers
    return numbers[0] * numbers[1];
}

public function main() {
    // Example usage
    int|error result = knotHash("input_level_10.txt");

    if result is int {
        io:println("Result: ", result);
    } else {
        io:println("Error: ", result.message());
    }
}
