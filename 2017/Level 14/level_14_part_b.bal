import ballerina/io;

function knotHash(string inputStr) returns string {
    int[] lengths = [];

    // Convert input string to ASCII codes and append the standard suffix
    foreach byte b in inputStr.toBytes() {
        lengths.push(<int> b);
    }
    int[] suffix = [17, 31, 73, 47, 23];
    // Manually append suffix elements to lengths
    foreach int s in suffix {
        lengths.push(s);
    }

    // Initialize the sparse hash
    int[] sparseHash = [];
    foreach int i in 0 ..< 256 {
        sparseHash.push(i);
    }

    int skipSize = 0;
    int pos = 0;

    // Perform 64 rounds
    foreach int _ in 0 ..< 64 {
        foreach int length in lengths {
            if (length > 1) {
                // Get the sublist to reverse
                int[] sublist = [];
                foreach int i in 0 ..< length {
                    sublist.push(sparseHash[(pos + i) % 256]);
                }
                // Reverse the sublist and assign it back
                sublist = sublist.reverse();
                // Place the reversed sublist back into the sparse hash
                foreach int i in 0 ..< length {
                    sparseHash[(pos + i) % 256] = sublist[i];
                }
            }
            // Update position and skip size
            pos = (pos + length + skipSize) % 256;
            skipSize += 1;
        }
    }

    // Compute the dense hash
    int[] denseHash = [];
    int index = 0;
    while index < 256 {
        int block = sparseHash[index];
        foreach int j in index + 1 ..< index + 16 {
            block ^= sparseHash[j];
        }
        denseHash.push(block);
        index += 16;
    }

    // Convert the dense hash to hexadecimal string
    string hexHash = "";
    foreach int val in denseHash {
        string hex = val.toHexString();
        if hex.length() == 1 {
            hex = "0" + hex;
        }
        hexHash += hex;
    }

    return hexHash;
}

// Convert a hexadecimal string to a binary string with exactly 128 bits
function hexToBin(string hexString) returns string {
    map<string> hexToBinMap = {
        "0": "0000", "1": "0001", "2": "0010", "3": "0011",
        "4": "0100", "5": "0101", "6": "0110", "7": "0111",
        "8": "1000", "9": "1001", "a": "1010", "b": "1011",
        "c": "1100", "d": "1101", "e": "1110", "f": "1111"
    };

    string binaryString = "";
    // Iterate over each character in the hex string
    foreach int i in 0 ..< hexString.length() {
        string hexChar = hexString.substring(i, i + 1).toLowerAscii(); // Get one character
        string binValue = hexToBinMap[hexChar] ?: "0000"; // Convert it to binary using the map
        binaryString += binValue;
    }

    return binaryString;
}

// Generate the grid and return it as a 2D array of integers (0 or 1)
function generateGrid(string keyString) returns int[][] {
    int[][] grid = [];
    
    // Iterate over 128 rows
    foreach int i in 0 ..< 128 {
        string rowInput = keyString + "-" + i.toString();
        string knotHashValue = knotHash(rowInput);  // Compute knot hash for each row
        string binaryRep = hexToBin(knotHashValue); // Convert hex hash to binary

        int[] row = [];
        // Convert the binary string to an array of integers
        foreach var c in binaryRep {
            if c == "1" {
                row.push(1);
            } else {
                row.push(0);
            }
        }
        grid.push(row);
    }
    
    return grid;
}

// Depth-first search function to mark connected regions
function dfs(int row, int col, int[][] grid, boolean[][] visited, int numRows, int numCols, int[][] directions) {
    if (row < 0 || row >= numRows || col < 0 || col >= numCols) {
        return;
    }
    if (visited[row][col] || grid[row][col] == 0) {
        return;
    }
    visited[row][col] = true;

    foreach var dir in directions {
        int newRow = row + dir[1];
        int newCol = col + dir[0];
        dfs(newRow, newCol, grid, visited, numRows, numCols, directions);
    }
}

// Count the number of regions in the grid
function countRegions(int[][] grid) returns int {
    int numRows = grid.length();
    int numCols = grid[0].length();
    int numRegions = 0;
    boolean[][] visited = [];

    // Initialize the visited array
    foreach int i in 0 ..< numRows {
        boolean[] rowVisited = [];
        foreach int _ in 0 ..< numCols {
            rowVisited.push(false);
        }
        visited.push(rowVisited);
    }

    // Directions for adjacent squares (up, down, left, right)
    int[][] directions = [
        [0, -1],  // Up
        [0, 1],   // Down
        [-1, 0],  // Left
        [1, 0]    // Right
    ];

    // Iterate over each cell in the grid
    foreach int i in 0 ..< numRows {
        foreach int j in 0 ..< numCols {
            if grid[i][j] == 1 && !visited[i][j] {
                numRegions += 1;
                dfs(i, j, grid, visited, numRows, numCols, directions);
            }
        }
    }

    return numRegions;
}

public function main() {
    string keyString = "jxqlasbh";  // Your input key string

    // Generate the grid
    int[][] grid = generateGrid(keyString);

    // Count the number of regions
    int numRegions = countRegions(grid);

    io:println("Total number of regions: " + numRegions.toString());
}
