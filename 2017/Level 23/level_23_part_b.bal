import ballerina/io;
import ballerina/regex;

public function main() returns error? {
    // Read the input from the file
    string content = check io:fileReadString("input_level_23.txt");

    // Split the content into lines using regex
    string[] lines = check regex:split(content, "\r?\n");

    // Initialize registers map with default values (all 0 except `a` which is set to 1 for Part Two)
    map<int> registers = { "a": 1 };

    // Initialize `b` and `c`
    int b = 0;
    int c = 0;

    // Process the instructions to find initial values for `b` and `c`
    foreach var line in lines {
        string[] tokens = check regex:split(line, "\\s+");
        if tokens.length() < 2 {
            continue; // Invalid instruction, skip
        }

        string instr = tokens[0];
        string X = tokens[1];
        string Y = tokens.length() > 2 ? tokens[2] : "";

        match instr {
            "set" => {
                if X == "b" {
                    b = getValue(Y, registers);
                } else if X == "c" {
                    c = getValue(Y, registers);
                }
            }
            "sub" => {
                if X == "c" {
                    int yVal = getValue(Y, registers);
                    c -= yVal; // Adjust `c` based on the instruction
                }
            }
            _ => {
                // Other instructions are ignored for now
            }
        }
    }

    // Apply the debug mode effect: adjust `b` and `c`
    b = b * 100 + 100000;
    c = b + 17000;

    // Initialize `h` to count non-prime numbers in the range [b, c] with a step of 17
    int h = 0;

    // Iterate through the range and count non-prime numbers
    int n = b;
    while n <= c {
        if !isPrime(n) {
            h += 1;
        }
        n += 17;  // Step of 17
    }

    // Output the final value of register `h`
    io:println("Final value in register h: ", h);
}

// Helper function to get the value of an operand (register or integer)
function getValue(string operand, map<int> registers) returns int {
    int|error intVal = int:fromString(operand);
    if intVal is int {
        return intVal;
    } else {
        return getRegisterValue(operand, registers);
    }
}

// Helper function to get the value of a register, defaulting to 0 if the register doesn't exist
function getRegisterValue(string regName, map<int> registers) returns int {
    int|() val = registers[regName];
    if val is int {
        return val;
    } else {
        registers[regName] = 0;
        return 0;
    }
}

// Helper function to check if a number is prime
function isPrime(int n) returns boolean {
    if n <= 1 {
        return false;
    }
    int i = 2;
    while i * i <= n {
        if n % i == 0 {
            return false;
        }
        i += 1;
    }
    return true;
}
