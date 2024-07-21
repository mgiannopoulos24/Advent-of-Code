#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>
#include <cctype>

// Function to parse and execute the assembly code
bool executeCode(int initialA, const std::vector<std::string>& instructions) {
    std::unordered_map<char, int> registers;
    registers['a'] = initialA;
    registers['b'] = 0;
    registers['c'] = 0;
    registers['d'] = 0;

    int pc = 0; // Program counter
    int outputCount = 0;
    int expected = 0;

    while (pc >= 0 && pc < instructions.size()) {
        std::istringstream iss(instructions[pc]);
        std::string op;
        iss >> op;
        if (op == "cpy") {
            std::string src;
            char dest;
            iss >> src >> dest;
            int value = std::isdigit(src[0]) ? std::stoi(src) : registers[src[0]];
            registers[dest] = value;
        } else if (op == "inc") {
            char reg;
            iss >> reg;
            registers[reg]++;
        } else if (op == "dec") {
            char reg;
            iss >> reg;
            registers[reg]--;
        } else if (op == "jnz") {
            std::string cond;
            int offset;
            iss >> cond >> offset;
            int condition = std::isdigit(cond[0]) ? std::stoi(cond) : registers[cond[0]];
            if (condition != 0) {
                pc += offset;
                continue;
            }
        } else if (op == "out") {
            std::string src;
            iss >> src;
            int value = std::isdigit(src[0]) ? std::stoi(src) : registers[src[0]];
            if (value != expected) return false;
            expected = 1 - expected; // Toggle between 0 and 1
            outputCount++;
            if (outputCount > 100) return true; // Check a reasonable number of outputs
        }
        pc++;
    }
    return false; // If we end without achieving the desired output
}

int main() {
    std::ifstream file("input_level_25.txt");
    if (!file.is_open()) {
        std::cerr << "Unable to open file" << std::endl;
        return EXIT_FAILURE;
    }

    // Read the assembly code
    std::vector<std::string> instructions;
    std::string line;
    while (std::getline(file, line)) {
        instructions.push_back(line);
    }
    file.close();

    // Try different initial values for register a
    for (int i = 1; ; ++i) {
        if (executeCode(i, instructions)) {
            std::cout << "The lowest positive integer to initialize register a is " << i << "." << std::endl;
            break;
        }
    }

    return EXIT_SUCCESS;
}
