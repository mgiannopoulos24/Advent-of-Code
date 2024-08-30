import ballerina/io;

function abs(int value) returns int {
    return value < 0 ? -value : value;
}

function manhattanDistance(int n) returns int {
    if (n == 1) {
        return 0;
    }

    // Determine the layer of the number
    int layer = 0;
    while ((2 * layer + 1) * (2 * layer + 1) < n) {
        layer += 1;
    }

    // Calculate the maximum value in the current layer
    int maxValInLayer = (2 * layer + 1) * (2 * layer + 1);
    int sideLength = 2 * layer;
    int stepsToReachLayer = layer;

    // Find the position within the layer
    int offset = maxValInLayer - n;
    int positionInSide = offset % sideLength;

    // Calculate the Manhattan Distance
    int distance = stepsToReachLayer + abs(positionInSide - layer);
    return distance;
}

public function main() {
    int puzzleInput = 277678; // Change to your puzzle input
    int result = manhattanDistance(puzzleInput);
    io:println("The Manhattan distance from square ", puzzleInput, " to the access port is ", result);
}
