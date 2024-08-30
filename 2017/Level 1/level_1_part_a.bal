import ballerina/io;
import ballerina/lang.'string as strings;

public function main() returns error? {
    // Read the input from a file named 'input.txt'
    string input = check io:fileReadString("input_level_1.txt");
    
    // Remove any leading/trailing whitespace
    input = strings:trim(input);
    
    int length = input.length();
    int sum = 0;

    // Loop through each character in the input string
    foreach int i in 0 ..< length {
        // Circular next index
        int nextIndex = (i + 1) % length;

        // Compare current character to the next character (circular)
        if input[i] == input[nextIndex] {
            // Convert char to string, then to int, and add to sum
            sum += check 'int:fromString(strings:substring(input, i, i + 1));
        }
    }

    // Output the result
    io:println("The captcha sum is: ", sum);
}
