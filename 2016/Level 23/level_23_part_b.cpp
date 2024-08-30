#include <iostream>
#include <fstream>
#include <sstream>
#include <unordered_map>
#include <vector>
#include <string>
#include <cstdlib>

using namespace std;

// Function to parse the instruction
pair<string, vector<string>> parse_instruction(const string& instruction) {
    istringstream iss(instruction);
    string instr;
    iss >> instr;
    
    vector<string> args;
    string arg;
    while (iss >> arg) {
        args.push_back(arg);
    }
    
    return {instr, args};
}

// Function to execute the program
int execute_program(const vector<string>& instructions, int initial_a) {
    unordered_map<char, int> registers{{'a', initial_a}, {'b', 0}, {'c', 0}, {'d', 0}};
    vector<pair<string, vector<string>>> program;
    
    // Parse all instructions
    for (const string& instr : instructions) {
        program.push_back(parse_instruction(instr));
    }
    
    int ip = 0;
    
    while (ip >= 0 && ip < static_cast<int>(program.size())) {
        auto [instr, args] = program[ip];
        
        if (instr == "cpy") {
            string x = args[0];
            string y = args[1];
            
            if (registers.find(y[0]) != registers.end()) {
                int value = (x[0] >= '0' && x[0] <= '9') || (x[0] == '-' && x[1] >= '0' && x[1] <= '9') ? stoi(x) : registers[x[0]];
                registers[y[0]] = value;
            }
        } else if (instr == "inc") {
            string x = args[0];
            if (registers.find(x[0]) != registers.end()) {
                registers[x[0]] += 1;
            }
        } else if (instr == "dec") {
            string x = args[0];
            if (registers.find(x[0]) != registers.end()) {
                registers[x[0]] -= 1;
            }
        } else if (instr == "jnz") {
            string x = args[0];
            string y = args[1];
            
            int x_val = (x[0] >= '0' && x[0] <= '9') || (x[0] == '-' && x[1] >= '0' && x[1] <= '9') ? stoi(x) : registers[x[0]];
            int y_val = (y[0] >= '0' && y[0] <= '9') || (y[0] == '-' && y[1] >= '0' && y[1] <= '9') ? stoi(y) : registers[y[0]];
            
            if (x_val != 0) {
                ip += y_val - 1;
            }
        } else if (instr == "tgl") {
            string x = args[0];
            int offset = (x[0] >= '0' && x[0] <= '9') || (x[0] == '-' && x[1] >= '0' && x[1] <= '9') ? stoi(x) : registers[x[0]];
            int tgl_idx = ip + offset;
            
            if (tgl_idx >= 0 && tgl_idx < static_cast<int>(program.size())) {
                auto& tgl_instr = program[tgl_idx];
                auto& [tgl_cmd, tgl_args] = tgl_instr;
                
                if (tgl_args.size() == 1) {
                    tgl_cmd = (tgl_cmd == "inc") ? "dec" : "inc";
                } else if (tgl_args.size() == 2) {
                    tgl_cmd = (tgl_cmd == "jnz") ? "cpy" : "jnz";
                }
            }
        }
        
        ip++;
    }
    
    return registers['a'];
}

// Main function
int main() {
    ifstream file("input_level_23.txt");
    vector<string> instructions;
    string line;
    
    while (getline(file, line)) {
        instructions.push_back(line);
    }
    
    int initial_a = 12;  // Updated initial value for Part Two
    int result = execute_program(instructions, initial_a);
    cout << result << endl;
    
    return 0;
}


// This took 32 minutes to run.