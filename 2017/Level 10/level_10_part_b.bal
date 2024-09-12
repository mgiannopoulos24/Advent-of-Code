import ballerina/io;

function knotHash(string inputFile) returns string|error {
    // Read the input from the text file
    string content = check io:fileReadString(inputFile);
    content = content.trim(); // Clean up leading/trailing whitespace

    // Convert the input string to an array of ASCII values (bytes) and cast them to int
    byte[] byteArray = content.toBytes();
    int[] lengths = [];
    foreach var b in byteArray {
        lengths.push(<int>b); // Explicitly cast each byte to int
    }

    // Add the standard suffix lengths to the end of the sequence
    int[] suffixLengths = [17, 31, 73, 47, 23];
    foreach int suffix in suffixLengths {
        lengths.push(suffix);
    }

    // Initialize the list of numbers and other variables
    int[] numbers = [];
    foreach int i in 0 ... 255 {
        numbers.push(i);
    }

    int currentPosition = 0;
    int skipSize = 0;
    int n = numbers.length();

    // Run the algorithm for 64 rounds
    foreach int round in 0..<64 {
        foreach int length in lengths {
            if length > n {
                continue;
            }

            // Reverse the section of the list
            int[] toReverse = [];
            foreach int i in 0..<length {
                toReverse.push(numbers[(currentPosition + i) % n]);
            }
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
    }

    // Create the dense hash by XORing each block of 16 numbers
    int[] denseHash = [];
    foreach int i in 0..<16 {
        int blockXor = numbers[i * 16];
        foreach int j in 1..<16 {
            blockXor ^= numbers[i * 16 + j];
        }
        denseHash.push(blockXor);
    }

    // Convert the dense hash to a hexadecimal string
    string result = "";
    foreach int num in denseHash {
        string hexValue = int:toHexString(num);
        if hexValue.length() == 1 {
            result += "0" + hexValue; // Ensure two hexadecimal digits
        } else {
            result += hexValue;
        }
    }

    return result;
}

public function main() {
    // Specify the path to the input file
    string filePath = "input_level_10.txt";
    
    // Call the knotHash function
    string|error result = knotHash(filePath);

    if result is string {
        io:println("Knot Hash: ", result);
    } else {
        io:println("Error: ", result.message());
    }
}
