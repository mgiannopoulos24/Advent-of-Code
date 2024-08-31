import ballerina/io;

// Function to get the adjacent sum of a cell
function getAdjacentSum(int x, int y, map<json> grid) returns int {
    int adjacentSum = 0;
    int[][] deltas = [[-1, -1], [0, -1], [1, -1], [-1, 0], [1, 0], [-1, 1], [0, 1], [1, 1]];

    foreach int[] delta in deltas {
        int dx = delta[0];
        int dy = delta[1];
        string key = "(" + (x + dx).toString() + "," + (y + dy).toString() + ")";
        json? value = grid[key];
        if (value is int) {
            adjacentSum += value;
        }
    }
    return adjacentSum;
}

// Function to find the first value larger than the input
function firstValueLargerThanInput(int puzzleInput) returns int {
    map<json> grid = {};
    grid["(0,0)"] = 1;

    int x = 0;
    int y = 0;
    int step = 1;
    int[][] directions = [[1, 0], [0, 1], [-1, 0], [0, -1]];

    while true {
        foreach int[] direction in directions {
            foreach int i in 0 ..< step {
                x += direction[0];
                y += direction[1];
                string key = "(" + x.toString() + "," + y.toString() + ")";
                int adjacentSum = getAdjacentSum(x, y, grid);
                grid[key] = adjacentSum;

                if (adjacentSum > puzzleInput) {
                    return adjacentSum;
                }
            }
            // Increase the step size after completing vertical directions
            if (direction == [0, 1] || direction == [0, -1]) {
                step += 1;
            }
        }
    }
}

// Main function to test the above function
public function main() {
    int puzzleInput = 277678;
    int result = firstValueLargerThanInput(puzzleInput);
    io:println(result);
}
