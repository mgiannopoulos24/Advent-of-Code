#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <unordered_map>
#include <string>

using namespace std;

// Function to check if a string is a number
bool isNumber(const string &s) {
    return !s.empty() && (isdigit(s[0]) || s[0] == '-' || s[0] == '+');
}

// Function to get the value of a parameter, which could be a register or a number
int getValue(const unordered_map<string, int> &registers, const string &param) {
    if (isNumber(param)) {
        return stoi(param);
    } else {
        return registers.at(param);
    }
}

int main() {
    // Initialize the registers
    unordered_map<string, int> registers = {{"a", 0}, {"b", 0}, {"c", 1}, {"d", 0}}; // c is initialized to 1
    
    // Read the input file
    ifstream inputFile("input_level_12.txt");
    if (!inputFile) {
        cerr << "Unable to open file input.txt";
        return 1;
    }

    vector<string> instructions;
    string line;
    while (getline(inputFile, line)) {
        instructions.push_back(line);
    }
    inputFile.close();

    // Execute the instructions
    for (size_t i = 0; i < instructions.size(); ++i) {
        stringstream ss(instructions[i]);
        string instr, x, y;
        ss >> instr >> x >> y;

        if (instr == "cpy") {
            registers[y] = getValue(registers, x);
        } else if (instr == "inc") {
            registers[x]++;
        } else if (instr == "dec") {
            registers[x]--;
        } else if (instr == "jnz") {
            if (getValue(registers, x) != 0) {
                i += getValue(registers, y) - 1; // -1 because of the upcoming increment of the loop
            }
        }
    }

    // Output the value of register 'a'
    cout << "The value in register a is: " << registers["a"] << endl;

    return 0;
}
