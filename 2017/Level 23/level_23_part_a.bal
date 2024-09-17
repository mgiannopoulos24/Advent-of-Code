import ballerina/io;
import ballerina/regex;

public function main() returns error? {
    // Read the input from the file
    string content = check io:fileReadString("input_level_23.txt");

    // Split the content into lines
    string[] lines = regex:split(content, "\r?\n");

    // Initialize the registers map
    // Registers a through h start at 0
    map<int> registers = {};

    // Initialize instruction pointer
    int ip = 0;

    // Count the number of times 'mul' is invoked
    int mulCount = 0;

    int numInstructions = lines.length();

    // Simulate the execution of the instructions
    while ip >= 0 && ip < numInstructions {
        string line = lines[ip];

        // Split the instruction into tokens
        string[] tokens = regex:split(line, "\\s+");
        if tokens.length() < 2 {
            // Invalid instruction; skip
            ip += 1;
            continue;
        }

        string instr = tokens[0];
        string X = tokens[1];
        string Y = "";
        if tokens.length() > 2 {
            Y = tokens[2];
        }

        match instr {
            "set" => {
                // set X Y
                int yVal = getValue(Y, registers);
                registers[X] = yVal;
                ip += 1;
            }
            "sub" => {
                // sub X Y
                int yVal = getValue(Y, registers);
                int xVal = getRegisterValue(X, registers);
                registers[X] = xVal - yVal;
                ip += 1;
            }
            "mul" => {
                // mul X Y
                int yVal = getValue(Y, registers);
                int xVal = getRegisterValue(X, registers);
                registers[X] = xVal * yVal;
                mulCount += 1;
                ip += 1;
            }
            "jnz" => {
                // jnz X Y
                int xVal = getValue(X, registers);
                if xVal != 0 {
                    int yVal = getValue(Y, registers);
                    ip += yVal;
                } else {
                    ip += 1;
                }
            }
            _ => {
                // Unknown instruction; skip
                io:println("Unknown instruction: ", instr);
                ip += 1;
            }
        }
    }

    // Output the number of times 'mul' is invoked
    io:println(mulCount);
}

// Function to get the value of an operand (register or integer)
function getValue(string operand, map<int> registers) returns int {
    int|error intVal = int:fromString(operand);
    if intVal is int {
        return intVal;
    } else {
        // Not an integer; get the value from the register
        return getRegisterValue(operand, registers);
    }
}

// Function to get the value of a register
function getRegisterValue(string regName, map<int> registers) returns int {
    int|() val = registers[regName];
    if val is int {
        return val;
    } else {
        // Initialize the register to 0 if it doesn't exist
        registers[regName] = 0;
        return 0;
    }
}
