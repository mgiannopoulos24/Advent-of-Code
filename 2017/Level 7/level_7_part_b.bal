import ballerina/io;
import ballerina/regex;

type Program record {
    string name;
    int weight;
    string[] supports;
};

public function main() {
    // Read input from the file
    var result = io:fileReadString("input_level_7.txt");
    string input;
    if result is io:Error {
        io:println("Error reading the input file: ", result.message());
        return;
    } else {
        input = result;
    }
    
    // Split input by lines
    string[] lines = regex:split(input, "\n");

    // Program data structures
    map<Program> programs = {};
    map<int> weights = {};
    map<string[]> supports = {};

    // Parse each line
    foreach var line in lines {
        if line == "" {
            continue;
        }

        // Match and parse the line
        string[] parts = regex:split(line, " -> ");
        string[] nameWeight = regex:split(parts[0], " ");

        // Extract name and weight
        string name = nameWeight[0];
        int weight;
        var weightResult = 'int:fromString(nameWeight[1].substring(1, nameWeight[1].length() - 1)); // Remove parentheses
        if weightResult is error {
            io:println("Error parsing the weight for ", name);
            return;
        } else {
            weight = weightResult;
        }
        
        // Extract supports if present
        string[] supportedPrograms = [];
        if parts.length() > 1 {
            supportedPrograms = regex:split(parts[1], ", ");
        }

        // Store the program
        Program program = {name: name, weight: weight, supports: supportedPrograms};
        programs[name] = program;
        weights[name] = weight;
        supports[name] = supportedPrograms;
    }

    // Part 1: Find the bottom program
    map<boolean> allProgramsSet = {};
    map<boolean> supportedProgramsSet = {};
    
    // Populate the maps to simulate sets
    foreach var programName in programs.keys() {
        allProgramsSet[programName] = true;
        foreach var supportedProgram in supports[programName] ?: [] {  // Handle nullable array
            supportedProgramsSet[supportedProgram] = true;
        }
    }
    
    // The bottom program is the one in allPrograms but not in supportedPrograms
    string bottomProgram = "";
    foreach var programName in allProgramsSet.keys() {
        if !supportedProgramsSet.hasKey(programName) {
            io:println("Part 1 - Bottom program: ", programName);
            bottomProgram = programName;
            break;
        }
    }

    // Part 2: Find the first imbalance and print it
    _ = findFirstImbalance(bottomProgram, programs, weights, supports);
}

// Recursive function to find and print only the first imbalance
function findFirstImbalance(string program, map<Program> programs, map<int> weights, map<string[]> supports) returns int? {
    string[] supported = supports[program] ?: [];
    
    // If the program doesn't support any others, return its own weight
    int programWeight = weights[program] ?: 0; // Handle nullable weight by providing a default value
    if supported.length() == 0 {
        return programWeight;
    }

    // Calculate the weights of all sub-towers
    map<int> subTowerWeights = {};
    foreach var subProgram in supported {
        int? subWeight = findFirstImbalance(subProgram, programs, weights, supports);
        if subWeight is () {
            return (); // If an imbalance was found in recursion, stop further checks
        }
        subTowerWeights[subProgram] = subWeight;
    }

    // Get the weights of sub-towers
    int[] weightValues = getValues(subTowerWeights);
    if allEqual(weightValues) {
        return programWeight + weightValues[0] * supported.length();
    } else {
        // Find the imbalance
        string wrongProgram = "";
        int correctWeight = 0;
        int wrongWeight = 0;
        
        foreach var [key, value] in subTowerWeights.entries() {
            int count = countOccurrences(weightValues, value);
            if count == 1 {
                wrongProgram = key;
                wrongWeight = value;
            } else {
                correctWeight = value;
            }
        }

        // Safely unwrap the weight of the wrong program
        int wrongProgramWeight = weights[wrongProgram] ?: 0;
        int weightDifference = correctWeight - wrongWeight;

        // Print the first imbalance and return '()' to stop further checks
        io:println("Part 2 - Program ", wrongProgram, " is unbalanced and should be ", (wrongProgramWeight + weightDifference));
        return ();
    }
}

// Utility function to check if all weights are equal
function allEqual(int[] values) returns boolean {
    int first = values[0];
    foreach var val in values {
        if val != first {
            return false;
        }
    }
    return true;
}

// Utility function to count occurrences of a value in an array
function countOccurrences(int[] arr, int value) returns int {
    int count = 0;
    foreach var val in arr {
        if val == value {
            count += 1;
        }
    }
    return count;
}

// Utility function to get all values from a map
function getValues(map<int> inputMap) returns int[] {
    int[] result = [];
    foreach var key in inputMap.keys() {  // Correct syntax for iterating over map keys
        result.push(inputMap[key] ?: 0);  // Push values, providing default if needed
    }
    return result;
}
