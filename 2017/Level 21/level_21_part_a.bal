import ballerina/io;
import ballerina/regex;

function rotateGrid(string[][] grid) returns string[][] {
    // Rotate the grid 90 degrees clockwise.
    int size = grid.length();
    string[][] rotatedGrid = [];
    foreach int i in 0 ..< size {
        string[] newRow = [];
        foreach int j in 0 ..< size {
            newRow.push(grid[size - 1 - j][i]);
        }
        rotatedGrid.push(newRow);
    }
    return rotatedGrid;
}

function flipGrid(string[][] grid) returns string[][] {
    // Flip the grid horizontally by manually reversing each row.
    string[][] flippedGrid = [];
    foreach string[] row in grid {
        flippedGrid.push(reverseArray(row));
    }
    return flippedGrid;
}

function reverseArray(string[] arr) returns string[] {
    // Manually reverse an array of strings.
    int len = arr.length();
    string[] reversedArr = [];
    foreach int i in 0 ..< len {
        reversedArr.push(arr[len - i - 1]);
    }
    return reversedArr;
}

function getVariants(string[][] grid) returns string[][][] {
    // Generate all rotations and flips of the grid.
    string[][][] variants = [];
    string[][] currentGrid = grid; // Create a mutable local copy of the grid

    foreach int _ in 0 ..< 4 {
        currentGrid = rotateGrid(currentGrid);
        variants.push(currentGrid);
        variants.push(flipGrid(currentGrid)); // Add flipped version
    }
    return variants;
}


function gridToString(string[][] grid) returns string {
    // Convert grid (list of strings) to string format used in rules.
    string result = "";
    foreach string[] row in grid {
        result += "." + stringJoin(row, ""); // Manually join each row without separator
    }
    return result;
}

function stringToGrid(string s) returns string[][] {
    // Convert string format from rules into a grid (list of strings).
    string[] rows = regex:split(s, "/");
    string[][] grid = [];
    foreach string row in rows {
        grid.push(splitToChars(row));
    }
    return grid;
}

function splitToChars(string s) returns string[] {
    // Helper function to split a string into an array of characters.
    return regex:split(s, "");
}

function stringJoin(string[] parts, string delimiter) returns string {
    // Manually join an array of strings with a delimiter.
    string result = "";
    foreach int i in 0 ..< parts.length() {
        result += parts[i];
        if i < parts.length() - 1 {
            result += delimiter;
        }
    }
    return result;
}

function parseInput(string filePath) returns map<string[][]>|error {
    // Parse the enhancement rules from the input file.
    map<string[][]> rules = {};
    string input = check io:fileReadString(filePath);

    string[] lines = regex:split(input, "\n");
    foreach string line in lines {
        if line.trim() == "" {
            continue;
        }
        string[] parts = regex:split(line.trim(), " => ");
        string pattern = parts[0];
        string result = parts[1];
        string[][] grid = stringToGrid(pattern);
        string[][] resultGrid = stringToGrid(result);

        string[][][] variants = getVariants(grid);
        foreach string[][] variant in variants {
            rules[gridToString(variant)] = resultGrid;
        }
    }
    return rules;
}

function enhance(string[][] grid, map<string[][]> rules) returns string[][] {
    // Apply the enhancement rules to the grid.
    int size = grid.length();
    int step = size % 2 == 0 ? 2 : 3;
    string[][] newGrid = [];

    foreach int i in 0 ..< size {
        if i % step != 0 {
            continue; // Manually step through the grid
        }
        // Create blocks of rows that will form the new grid
        string[] newRows = [];

        foreach int j in 0 ..< size {
            if j % step != 0 {
                continue; // Manually step through the grid
            }
            string[][] block = [];
            foreach int k in 0 ..< step {
                string[] row = grid[i + k]; // row is an array of strings (individual characters)
                // Ensure j + step does not exceed the length of the row
                int end = (j + step) < row.length() ? (j + step) : row.length();
                string[] slicedRow = row.slice(j, end); // Slice the row
                block.push(slicedRow); // Add the sliced row (characters) to the block
            }
            string blockStr = gridToString(block);
            string[][]? enhancedBlock = rules[blockStr];

            if enhancedBlock is string[][] {
                // Add the enhanced block to the new rows
                foreach int k in 0 ..< (step + 1) {
                    if newRows.length() > k {
                        newRows[k] = newRows[k] + stringJoin(enhancedBlock[k], "");
                    } else {
                        newRows.push(stringJoin(enhancedBlock[k], ""));
                    }
                }
            }
        }

        foreach string row in newRows {
            newGrid.push(regex:split(row, ""));
        }
    }
    return newGrid;
}




function countOnPixels(string[][] grid) returns int {
    // Count the number of pixels that are on ('#') in the grid.
    int count = 0;
    foreach string[] row in grid {
        foreach string ch in row {
            if ch == "#" {
                count += 1;
            }
        }
    }
    return count;
}

function fractalArt(string filePath, int iterations) returns int|error {
    // Main function to solve the fractal art problem.
    map<string[][]> rules = check parseInput(filePath);
    string[][] grid = stringToGrid(".#./..#/###"); // Initial grid

    foreach int _ in 0 ..< iterations {
        grid = enhance(grid, rules);
    }

    return countOnPixels(grid);
}

// Usage example
public function main() returns error? {
    string filePath = "input_level_21.txt";
    int result = check fractalArt(filePath, 5);
    io:println("Number of pixels on after 5 iterations: ", result);
}
