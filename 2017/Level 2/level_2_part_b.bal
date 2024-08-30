import ballerina/io;
import ballerina/regex;

public function main() returns error? {
    // Read the input from a file named 'input.txt'
    string input = check io:fileReadString("input_level_2.txt");
    
    // Split the input into rows
    string[] rows = regex:split(input, "\n");

    int totalSum = 0;

    // Process each row
    foreach string row in rows {
        // Trim whitespace and check if the row is empty
        string trimmedRow = row.trim();
        if trimmedRow == "" {
            continue;
        }

        // Split the row into numbers using regex to handle multiple spaces
        string[] numbersStr = regex:split(trimmedRow, "\\s+");
        int[] numbers = [];

        // Convert each number from string to integer
        foreach string numStr in numbersStr {
            // Trim each number string and ignore empty strings
            string trimmedNumStr = numStr.trim();
            if trimmedNumStr != "" {
                int num = check 'int:fromString(trimmedNumStr);
                numbers.push(num);
            }
        }

        // Find the two numbers where one evenly divides the other
        boolean found = false;
        foreach int i in 0..<numbers.length() {
            foreach int j in 0..<numbers.length() {
                if i != j && numbers[i] % numbers[j] == 0 {
                    totalSum += numbers[i] / numbers[j];
                    found = true;
                    break;
                }
            }
            if found {
                break;
            }
        }
    }

    // Output the result
    io:println("The sum of each row's result is: ", totalSum);
}
