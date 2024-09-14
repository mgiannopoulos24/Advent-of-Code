import ballerina/io;
import ballerina/regex;

type Registers map<int>;

// Function to get the value, either from a register or an integer.
function getValue(string operand, Registers regs) returns int {
    int|error val = int:fromString(operand);
    if (val is int) {
        return val;
    } else {
        return regs[operand] ?: 0; // Default to 0 if the register is not initialized
    }
}

public function main() returns error? {
    // Read instructions from input file
    string instructions = check io:fileReadString("input_level_18.txt");
    
    // Split instructions into lines
    string[] lines = regex:split(instructions, "\n");

    // Initialize registers and other variables
    Registers regs = {};
    int sound = 0; // To store the last sound played
    int pc = 0;    // Program counter to keep track of the current instruction
    
    // Loop until we find the first rcv with a non-zero value
    while pc >= 0 && pc < lines.length() {
        // Split each instruction line into components (operation and operands)
        string[] parts = regex:split(lines[pc], " ");
        if parts.length() == 0 || parts[0] == "" {
            // Skip empty lines or invalid instructions
            pc += 1;
            continue;
        }

        string operation = parts[0];
        string X = parts.length() > 1 ? parts[1] : "";
        string Y = parts.length() > 2 ? parts[2] : "";

        // Execute the instruction based on the operation
        match operation {
            "snd" => {
                sound = getValue(X, regs); // Store the value to sound
            }
            "set" => {
                if X != "" && Y != "" {
                    regs[X] = getValue(Y, regs); // Set register X to the value of Y
                }
            }
            "add" => {
                if X != "" && Y != "" {
                    regs[X] = (regs[X] ?: 0) + getValue(Y, regs); // Add Y to register X
                }
            }
            "mul" => {
                if X != "" && Y != "" {
                    regs[X] = (regs[X] ?: 0) * getValue(Y, regs); // Multiply X by Y
                }
            }
            "mod" => {
                if X != "" && Y != "" {
                    regs[X] = (regs[X] ?: 0) % getValue(Y, regs); // X = X % Y
                }
            }
            "rcv" => {
                if getValue(X, regs) != 0 {
                    // When rcv is executed with a non-zero X, recover the sound and terminate
                    io:println("Recovered frequency: ", sound);
                    break;
                }
            }
            "jgz" => {
                if getValue(X, regs) > 0 {
                    // Jump Y steps if X > 0
                    pc += getValue(Y, regs) - 1;
                }
            }
            // Default case if no matching instruction
            _ => {
                io:println("Unknown instruction: ", operation);
            }
        }
        
        // Move to the next instruction
        pc += 1;
    }
}
