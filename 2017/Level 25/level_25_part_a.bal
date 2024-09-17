import ballerina/io;
import ballerina/regex;

type Action record {
    int write;
    string move;
    string nextState;
};

type State map<Action>;

type Blueprint record {
    string startState;
    int steps;
    map<State> states;
};

function runTuringMachine(Blueprint blueprint) returns int {
    map<int> tape = {};
    int cursor = 0;
    string state = blueprint.startState;
    int stepCount = blueprint.steps;

    foreach int _ in 0 ..< stepCount {
        // Handle optional int from the tape
        int currentValue = 0;
        int? optionalValue = tape[cursor.toString()];
        if optionalValue is int {
            currentValue = optionalValue;
        }

        // Safely access the states and action
        State? stateOptional = blueprint.states[state]; // Check if the state exists
        if stateOptional is State {
            Action? actionOptional = stateOptional.get(currentValue.toString()); // Get the action based on the current value
            if actionOptional is Action {
                Action action = actionOptional;

                // Write the value
                tape[cursor.toString()] = action.write;

                // Move the cursor
                if action.move == "right" {
                    cursor += 1;
                } else {
                    cursor -= 1;
                }

                // Transition to the next state
                state = action.nextState;
            } else {
                io:println("Invalid action encountered");
                return 0;
            }
        } else {
            io:println("Invalid state encountered");
            return 0;
        }
    }

    // Calculate the checksum (number of 1's on the tape)
    int checksum = 0;
    foreach var key in tape.keys() {
        int? optionalValue = tape[key];
        if optionalValue is int {
            if optionalValue == 1 {
                checksum += 1;
            }
        }
    }


    return checksum;
}





function parseBlueprint(string filePath) returns Blueprint|error {
    string content = check io:fileReadString(filePath);
    string[] lines = regex:split(content, "\\n");

    // Initialize the Blueprint with default values
    Blueprint blueprint = {
        startState: "",
        steps: 0,
        states: {}
    };

    // Parse the start state and number of steps
    string[] startStateLineParts = regex:split(lines[0], " ");
    blueprint.startState = startStateLineParts[startStateLineParts.length() - 1].substring(0, 1);

    string[] stepLineParts = regex:split(lines[1], " ");
    blueprint.steps = check 'int:fromString(stepLineParts[stepLineParts.length() - 2]);

    map<State> states = {};

    int idx = 3;
    while idx < lines.length() {
        string[] stateLineParts = regex:split(lines[idx], " ");
        string stateName = stateLineParts[stateLineParts.length() - 1].substring(0, 1);

        // Parsing action for value 0
        string[] writeLineParts0 = regex:split(lines[idx + 2], " ");
        string[] moveLineParts0 = regex:split(lines[idx + 3], " ");
        string[] nextStateLineParts0 = regex:split(lines[idx + 4], " ");

        Action value0 = {
            write: check 'int:fromString(writeLineParts0[writeLineParts0.length() - 1].substring(0, 1)),
            move: moveLineParts0[moveLineParts0.length() - 1].substring(0, moveLineParts0[moveLineParts0.length() - 1].length() - 1),
            nextState: nextStateLineParts0[nextStateLineParts0.length() - 1].substring(0, 1)
        };

        // Parsing action for value 1
        string[] writeLineParts1 = regex:split(lines[idx + 6], " ");
        string[] moveLineParts1 = regex:split(lines[idx + 7], " ");
        string[] nextStateLineParts1 = regex:split(lines[idx + 8], " ");

        Action value1 = {
            write: check 'int:fromString(writeLineParts1[writeLineParts1.length() - 1].substring(0, 1)),
            move: moveLineParts1[moveLineParts1.length() - 1].substring(0, moveLineParts1[moveLineParts1.length() - 1].length() - 1),
            nextState: nextStateLineParts1[nextStateLineParts1.length() - 1].substring(0, 1)
        };

        State state = {};
        state["0"] = value0;
        state["1"] = value1;
        states[stateName] = state;

        idx += 10;
    }

    blueprint.states = states;
    return blueprint;
}


public function main() {
    Blueprint|error blueprint = parseBlueprint("input_level_25.txt");

    if blueprint is Blueprint {
        int checksum = runTuringMachine(blueprint);
        io:println("Diagnostic checksum: ", checksum);
    } else {
        io:println("Error parsing blueprint: ", blueprint);
    }
}
