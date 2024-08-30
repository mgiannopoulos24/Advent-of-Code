import ballerina/io;
import ballerina/regex;

public function main() returns error? {
    // Read the input from a file named 'input.txt'
    string input = check io:fileReadString("input_level_2.txt");
    
    // Split the input into rows
    string[] rows = regex:split(input, "\n");

    int checksum = 0;

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

        // Check if the row has numbers
        if numbers.length() > 0 {
            int maxVal = numbers[0];
            int minVal = numbers[0];

            // Find the maximum and minimum values in the row
            foreach int num in numbers {
                if num > maxVal {
                    maxVal = num;
                }
                if num < minVal {
                    minVal = num;
                }
            }

            // Calculate the difference and add to checksum
            int difference = maxVal - minVal;
            checksum += difference;
        }
    }

    // Output the result
    io:println("The spreadsheet's checksum is: ", checksum);
}
