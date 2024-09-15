import ballerina/io;
import ballerina/regex;

public function main() returns error? {
    // Read the input from the file
    string content = check io:fileReadString("input_level_22.txt");

    // Split the content into lines
    string[] lines = regex:split(content, "\r?\n");

    // Initialize the infected nodes map
    // Key format: "x,y", Value: true (infected)
    map<boolean> infectedNodes = {};

    int height = lines.length();
    int width = 0;
    if height > 0 {
        width = lines[0].length();
    }
    int centerY = (height - 1) / 2;
    int centerX = (width - 1) / 2;

    // Populate the infected nodes map based on the input grid
    foreach int y in 0 ..< height {
        string line = lines[y];
        foreach int x in 0 ..< line.length() {
            string ch = line.substring(x, x + 1);
            if ch == "#" {
                // Node is infected
                int dx = x - centerX;
                int dy = y - centerY;
                string key = dx.toString() + "," + dy.toString();
                infectedNodes[key] = true;
            }
        }
    }

    // Virus carrier starts at (0,0), facing up (dirX=0, dirY=-1)
    int posX = 0;
    int posY = 0;
    int dirX = 0;
    int dirY = -1;

    int infectionsCaused = 0;
    int bursts = 10000;

    // Simulate the virus carrier for the specified number of bursts
    foreach int i in 1 ... bursts {
        // Get the current node's status
        string key = posX.toString() + "," + posY.toString();
        boolean infected = infectedNodes.hasKey(key);

        // Determine new direction based on the current node's status
        int tempDirX = dirX;
        int tempDirY = dirY;
        if infected {
            // Turn right
            dirX = -tempDirY;
            dirY = tempDirX;
        } else {
            // Turn left
            dirX = tempDirY;
            dirY = -tempDirX;
        }

        // Modify the node's infection status
        if infected {
            // Clean the node
            // infectedNodes.remove(key) returns boolean|error
            boolean|error removeResult = infectedNodes.remove(key);
            if removeResult is error {
                io:println("Error removing key: ", removeResult.message());
                // Handle the error as needed (e.g., break the loop, continue, etc.)
            }
            // If needed, you can use the removed value: 'boolean removedValue = removeResult;'
        } else {
            // Infect the node and increment the infection counter
            infectedNodes[key] = true;
            infectionsCaused += 1;
        }

        // Move forward to the next node
        posX += dirX;
        posY += dirY;
    }

    // Output the number of bursts that caused a node to become infected
    io:println(infectionsCaused);
}
