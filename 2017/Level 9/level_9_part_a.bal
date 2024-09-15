import ballerina/io;

function calculateTotalScore(string inputStream) returns int {
    int totalScore = 0;
    int currentScore = 0;
    boolean inGarbage = false;
    boolean ignoreNext = false;

    // Iterate over each character in the string
    foreach var char in inputStream {
        if (ignoreNext) {
            ignoreNext = false;
            continue;
        }

        if (inGarbage) {
            if (char == "!") {
                ignoreNext = true;
            } else if (char == ">") {
                inGarbage = false;
            }
        } else {
            if (char == "{") {
                currentScore += 1;
            } else if (char == "}") {
                totalScore += currentScore;
                currentScore -= 1;
            } else if (char == "<") {
                inGarbage = true;
            }
        }
    }

    return totalScore;
}

public function main() {
    // Handle file reading errors
    string|error inputStream = io:fileReadString("input_level_9.txt");

    if (inputStream is string) {
        // Calculate total score
        int result = calculateTotalScore(inputStream);
        io:println("Total score: ", result);
    } else {
        io:println("Error reading file: ", inputStream);
    }
}
