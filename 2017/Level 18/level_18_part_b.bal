import ballerina/io;
import ballerina/regex;

type Registers map<int>;

// Record to hold the program state
type ProgramState record {
    Registers registers;
    int pc;
    int sendCount;
    int[] sendQueue;
    int[] recvQueue;
    boolean waiting;
};

// Function to get the value, either from a register or an integer.
function getValue(string operand, Registers regs) returns int {
    int|error val = int:fromString(operand);
    if (val is int) {
        return val;
    } else {
        return regs[operand] ?: 0; // Default to 0 if the register is not initialized
    }
}

// Function to simulate one program's execution and return updated state
function runProgram(string[] instructions, ProgramState state, int programId) returns ProgramState {
    while state.pc >= 0 && state.pc < instructions.length() {
        string[] parts = regex:split(instructions[state.pc], " ");
        string operation = parts[0];
        string X = parts.length() > 1 ? parts[1] : "";
        string Y = parts.length() > 2 ? parts[2] : "";

        match operation {
            "snd" => {
                // Send a value to the other program's queue
                state.sendQueue.push(getValue(X, state.registers));
                if (programId == 1) {
                    state.sendCount += 1; // Increment the send count for program 1
                }
            }
            "set" => {
                if X != "" && Y != "" {
                    state.registers[X] = getValue(Y, state.registers);
                }
            }
            "add" => {
                if X != "" && Y != "" {
                    state.registers[X] = (state.registers[X] ?: 0) + getValue(Y, state.registers);
                }
            }
            "mul" => {
                if X != "" && Y != "" {
                    state.registers[X] = (state.registers[X] ?: 0) * getValue(Y, state.registers);
                }
            }
            "mod" => {
                if X != "" && Y != "" {
                    state.registers[X] = (state.registers[X] ?: 0) % getValue(Y, state.registers);
                }
            }
            "rcv" => {
                if state.recvQueue.length() > 0 {
                    // If there's something in the queue, receive it
                    state.registers[X] = state.recvQueue.shift();
                } else {
                    // If the queue is empty, this program is blocked
                    state.waiting = true;
                    return state; // Return the state to indicate that the program is waiting for a message
                }
            }
            "jgz" => {
                if getValue(X, state.registers) > 0 {
                    state.pc += getValue(Y, state.registers) - 1;
                }
            }
            _ => {
                io:println("Unknown instruction: ", operation);
            }
        }

        // Move to the next instruction
        state.pc += 1;
    }

    // Return the modified state
    return state;
}

public function main() returns error? {
    // Read instructions from input file
    string instructionsText = check io:fileReadString("input_level_18.txt");
    string[] instructions = regex:split(instructionsText, "\n");

    // Initialize registers for both programs
    ProgramState program0State = {
        registers: {p: 0},
        pc: 0,
        sendCount: 0,
        sendQueue: [],
        recvQueue: [],
        waiting: false
    };
    
    ProgramState program1State = {
        registers: {p: 1},
        pc: 0,
        sendCount: 0,
        sendQueue: [],
        recvQueue: [],
        waiting: false
    };

    // Simulate both programs
    while true {
        // Run both programs and update their states
        program0State = runProgram(instructions, program0State, 0);
        program1State = runProgram(instructions, program1State, 1);

        // Transfer messages between queues
        if program0State.sendQueue.length() > 0 {
            program1State.recvQueue.push(program0State.sendQueue.shift());
        }
        if program1State.sendQueue.length() > 0 {
            program0State.recvQueue.push(program1State.sendQueue.shift());
        }

        // If both programs are waiting and their queues are empty, terminate
        if program0State.waiting && program1State.waiting && program0State.recvQueue.length() == 0 && program1State.recvQueue.length() == 0 {
            break;
        }
    }

    // Output the result: how many times program 1 sent a value
    io:println("Program 1 sent a value ", program1State.sendCount, " times.");
}
