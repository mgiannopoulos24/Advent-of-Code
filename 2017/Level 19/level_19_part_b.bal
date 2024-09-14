import ballerina/io;
import ballerina/regex;

// Define the Direction enum
enum Direction {
    UP, DOWN, LEFT, RIGHT
}

public function main() returns error? {
    // Read the routing diagram from the input file
    string diagramText = check io:fileReadString("input_level_19.txt");

    // Split the diagram into lines (rows of the grid)
    string[] diagram = regex:split(diagramText, "\n");

    // Initialize the starting point: find the first | in the first row
    int? colOpt = diagram[0].indexOf("|");
    if colOpt is () {
        io:println("Error: Starting point not found!");
        return;
    }
    int col = colOpt;

    int row = 0;
    Direction direction = DOWN; // Start by going down
    string letters = ""; // To store the collected letters
    int steps = 0; // To track the number of steps taken

    while true {
        // Get the current character at the packet's position
        string currentChar = diagram[row].substring(col, col + 1);

        if currentChar == "|" || currentChar == "-" {
            // Continue moving in the current direction
            if direction == UP {
                row -= 1;
            } else if direction == DOWN {
                row += 1;
            } else if direction == LEFT {
                col -= 1;
            } else if direction == RIGHT {
                col += 1;
            }
        } else if currentChar == "+" {
            // Change direction at intersections
            if direction == UP || direction == DOWN {
                // We are moving vertically, so check left and right
                if col > 0 && (diagram[row].substring(col - 1, col) == "-" || regex:matches(diagram[row].substring(col - 1, col), "[A-Z]")) {
                    direction = LEFT;
                    col -= 1;
                } else if col < diagram[row].length() - 1 && (diagram[row].substring(col + 1, col + 2) == "-" || regex:matches(diagram[row].substring(col + 1, col + 2), "[A-Z]")) {
                    direction = RIGHT;
                    col += 1;
                }
            } else if direction == LEFT || direction == RIGHT {
                // We are moving horizontally, so check up and down
                if (row > 0 &&  (diagram[row - 1].substring(col, col + 1) == "|" || regex:matches(diagram[row - 1].substring(col, col + 1), "[A-Z]"))) {
                    direction = UP;
                    row -= 1;               
                }  else if (row < diagram.length() - 1 && (diagram[row + 1].substring(col, col + 1) == "|" || regex:matches(diagram[row + 1].substring(col, col + 1), "[A-Z]"))) {
                    direction = DOWN;
                    row += 1;
                }
            }
        } else if regex:matches(currentChar, "[A-Z]") {
            // Collect letters
            letters += currentChar;

            // Continue moving in the current direction
            if direction == UP {
                row -= 1;
            } else if direction == DOWN {
                row += 1;
            } else if direction == LEFT {
                col -= 1;
            } else if direction == RIGHT {
                col += 1;
            }
        } else {
            // We reached the end of the path
            break;
        }

        // Increment the step count
        steps += 1;
    }

    // Output the collected letters and the total steps taken
    io:println("Total steps: ", steps);
}
