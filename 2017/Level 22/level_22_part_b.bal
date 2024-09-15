import ballerina/io;
import ballerina/regex;

public function main() returns error? {
    // Read the input from the file
    string content = check io:fileReadString("input_level_22.txt");

    // Split the content into lines
    string[] lines = regex:split(content, "\r?\n");

    // Initialize the nodeStates map
    // Key format: "x,y", Value: state ("weakened", "infected", "flagged")
    map<string> nodeStates = {};

    int height = lines.length();
    int width = 0;
    if height > 0 {
        width = lines[0].length();
    }
    int centerY = (height - 1) / 2;
    int centerX = (width - 1) / 2;

    // Populate the nodeStates map based on the input grid
    foreach int y in 0 ..< height {
        string line = lines[y];
        foreach int x in 0 ..< line.length() {
            string ch = line.substring(x, x + 1);
            if ch == "#" {
                // Node is infected
                int dx = x - centerX;
                int dy = y - centerY;
                string key = dx.toString() + "," + dy.toString();
                nodeStates[key] = "infected";
            }
        }
    }

    // Virus carrier starts at (0,0), facing up (dirX=0, dirY=-1)
    int posX = 0;
    int posY = 0;
    int dirX = 0;
    int dirY = -1;

    int infectionsCaused = 0;
    int bursts = 10000000;

    // Simulate the virus carrier for the specified number of bursts
    foreach int i in 1 ... bursts {
        // Get the current node's state
        string key = posX.toString() + "," + posY.toString();
        string? state = nodeStates[key]; // state is 'string|()'

        // Decide direction based on the current node's state
        int tempDirX = dirX;
        int tempDirY = dirY;
        if state == () {
            // Node is clean; turn left
            dirX = tempDirY;
            dirY = -tempDirX;
        } else if state == "weakened" {
            // Node is weakened; do not turn
            // dirX and dirY remain the same
        } else if state == "infected" {
            // Node is infected; turn right
            dirX = -tempDirY;
            dirY = tempDirX;
        } else if state == "flagged" {
            // Node is flagged; reverse direction
            dirX = -dirX;
            dirY = -dirY;
        }

        // Modify the node's state
        if state == () {
            // Node is clean; it becomes weakened
            nodeStates[key] = "weakened";
        } else if state == "weakened" {
            // Node becomes infected
            nodeStates[key] = "infected";
            infectionsCaused += 1;
        } else if state == "infected" {
            // Node becomes flagged
            nodeStates[key] = "flagged";
        } else if state == "flagged" {
            // Node becomes clean; remove it from the map
            string|error removeResult = nodeStates.remove(key);
            if removeResult is error {
                io:println("Error removing key: ", removeResult.message());
                // Handle the error as needed
            }
        }

        // Move forward to the next node
        posX += dirX;
        posY += dirY;
    }

    // Output the number of bursts that caused a node to become infected
    io:println(infectionsCaused);
}
