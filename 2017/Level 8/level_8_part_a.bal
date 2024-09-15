import ballerina/io;
import ballerina/regex;

public function main() returns error? {
    // Read the input from the file
    string input = check io:fileReadString("input_level_8.txt");

    // Initialize a map to store the register values
    map<int?> registers = {};

    // Split the input by new lines using regex:split
    string[] instructions = regex:split(input, "\n");

    // Process each instruction
    foreach string instruction in instructions {
        if instruction.trim() == "" {
            continue; // Skip any empty lines
        }

        // Split the instruction into components using regex:split
        string[] parts = regex:split(instruction, " ");

        string regToModify = parts[0];
        string operation = parts[1];
        int value = check int:fromString(parts[2]);
        string conditionReg = parts[4];
        string conditionOp = parts[5];
        int conditionValue = check int:fromString(parts[6]);

        // Ensure the condition register is initialized
        int conditionRegValue = 0;
        if registers.hasKey(conditionReg) {
            conditionRegValue = registers[conditionReg].clone() ?: 0;
        } else {
            registers[conditionReg] = 0;
        }

        // Ensure the register to modify is initialized
        int regValue = 0;
        if registers.hasKey(regToModify) {
            regValue = registers[regToModify].clone() ?: 0;
        } else {
            registers[regToModify] = 0;
        }

        // Check if the condition is met
        boolean conditionMet = false;

        if conditionOp == ">" {
            conditionMet = conditionRegValue > conditionValue;
        } else if conditionOp == "<" {
            conditionMet = conditionRegValue < conditionValue;
        } else if conditionOp == ">=" {
            conditionMet = conditionRegValue >= conditionValue;
        } else if conditionOp == "<=" {
            conditionMet = conditionRegValue <= conditionValue;
        } else if conditionOp == "==" {
            conditionMet = conditionRegValue == conditionValue;
        } else if conditionOp == "!=" {
            conditionMet = conditionRegValue != conditionValue;
        }

        // If the condition is met, modify the register
        if conditionMet {
            if operation == "inc" {
                regValue += value;
            } else if operation == "dec" {
                regValue -= value;
            }
            // Update the register in the map
            registers[regToModify] = regValue;
        }
    }

    // Find the largest value in the registers
    int largestValue = -2147483648; // Initialize to minimum possible int
    foreach var key in registers.keys() {
        int regValue = registers[key].clone() ?: 0;
        if regValue > largestValue {
            largestValue = regValue;
        }
    }

    // Output the largest value in any register
    io:println("The largest value in any register is: ", largestValue);
}
