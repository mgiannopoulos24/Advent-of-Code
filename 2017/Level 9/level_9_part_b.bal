import ballerina/io;

function calculateTotalScoreAndGarbageCount(string inputStream) returns [int, int] {
    int totalScore = 0;
    int currentScore = 0;
    boolean inGarbage = false;
    boolean ignoreNext = false;
    int garbageCharCount = 0;

    // Iterate over each character in the string
    foreach var char in inputStream {
        if (ignoreNext) {
            ignoreNext = false;
            continue;
        }

        if (inGarbage) {
            if (char == "!") {
                ignoreNext = true;  // Skip the next character
            } else if (char == ">") {
                inGarbage = false;  // Garbage ends
            } else {
                // Count valid garbage characters
                garbageCharCount += 1;
            }
        } else {
            if (char == "{") {
                currentScore += 1;  // Open a new group
            } else if (char == "}") {
                totalScore += currentScore;  // Close the group, add to total score
                currentScore -= 1;
            } else if (char == "<") {
                inGarbage = true;  // Start of garbage
            }
        }
    }

    // Return total score and garbage character count
    return [totalScore, garbageCharCount];
}

public function main() {
    // Handle file reading errors
    string|error inputStream = io:fileReadString("input_level_9.txt");

    if (inputStream is string) {
        // Calculate total score and garbage character count
        int totalScore;
        int garbageCharCount;
        [totalScore, garbageCharCount] = calculateTotalScoreAndGarbageCount(inputStream);
        
        io:println("Garbage character count: ", garbageCharCount);
    } else {
        io:println("Error reading file: ", inputStream);
    }
}
