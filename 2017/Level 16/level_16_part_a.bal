import ballerina/io;
import ballerina/regex;
import ballerina/lang.'int;    // For 'int:fromString

// Function to perform spin
function spin(int x, string[] programs) returns string[] {
    if (x <= 0 || x > programs.length()) { 
        return programs;
    }
    // Slice the last x elements
    string[] front = programs.slice(programs.length() - x, programs.length());
    // Slice the first (length - x) elements
    string[] back = programs.slice(0, programs.length() - x);
    return [...front, ...back];
}

// Function to perform exchange
function exchange(int a, int b, string[] programs) returns string[] {
    string temp = programs[a];
    programs[a] = programs[b];
    programs[b] = temp;
    return programs;
}

// Function to perform partner
function partner(string a, string b, string[] programs) returns string[] {
    int indexA = -1;
    int indexB = -1;
    foreach int i in 0 ..< programs.length() {
        string prog = programs[i];
        if (prog == a) {
            indexA = i;
        }
        if (prog == b) {
            indexB = i;
        }
        if (indexA != -1 && indexB != -1) {
            break;
        }
    }
    if (indexA != -1 && indexB != -1) {
        string temp = programs[indexA];
        programs[indexA] = programs[indexB];
        programs[indexB] = temp;
    }
    return programs;
}

public function main() returns error? {
    // Initialize programs a through p
    string[] programs = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p"];

    // Read dance moves from input.txt
    string danceMovesContent = check io:fileReadString("input_level_16.txt");

    // Split dance moves by comma using regex:split
    string[] danceMoves = regex:split(danceMovesContent, ",");

    // Iterate through each dance move and perform it
    foreach string move in danceMoves {
        if (move.startsWith("s")) {
            // Spin move
            string xStr = move.substring(1);
            int x = check 'int:fromString(xStr); 
            programs = spin(x, programs);
        } else if (move.startsWith("x")) {
            // Exchange move
            string[] parts = regex:split(move.substring(1), "/");
            int a = check 'int:fromString(parts[0]); 
            int b = check 'int:fromString(parts[1]); 
            programs = exchange(a, b, programs);
        } else if (move.startsWith("p")) {
            // Partner move
            string[] parts = regex:split(move.substring(1), "/");
            string a = parts[0];
            string b = parts[1];
            programs = partner(a, b, programs);
        } else {
            io:println("Unknown move: ", move);
        }
    }

    // Manually concatenate the final order of programs without using string:join
    string finalOrder = "";
    foreach string program in programs {
        finalOrder += program; // Manually concatenate each program
    }

    io:println("Final order of programs: ", finalOrder);
}
