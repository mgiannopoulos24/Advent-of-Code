import ballerina/io;
import ballerina/regex;

public function main() {
    // Read input directions from the file
    string[] directions = readDirectionsFromFile("input_level_11.txt");

    // Initialize cube coordinates (x, y, z)
    int x = 0;
    int y = 0;
    int z = 0;

    // Process each direction
    foreach string direction in directions {
        match direction {
            "n" => {
                y += 1;
                z -= 1;
            }
            "ne" => {
                x += 1;
                z -= 1;
            }
            "se" => {
                x += 1;
                y -= 1;
            }
            "s" => {
                z += 1;
                y -= 1;
            }
            "sw" => {
                x -= 1;
                z += 1;
            }
            "nw" => {
                x -= 1;
                y += 1;
            }
            _ => {
                io:println("Invalid direction: " + direction);
            }
        }
    }

    // Calculate the distance from the origin (0, 0, 0)
    int distance = (abs(x) + abs(y) + abs(z)) / 2;

    io:println("The fewest number of steps required: ", distance);
}

// Function to calculate absolute value of an integer
function abs(int value) returns int {
    return value < 0 ? -value : value;
}

// Function to read directions from a file and split by commas using regex
function readDirectionsFromFile(string filePath) returns string[] {
    string content = checkpanic io:fileReadString(filePath);
    // Split the content by commas using regex
    return regex:split(content, ",");
}
