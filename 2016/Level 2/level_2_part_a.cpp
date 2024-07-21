#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>

// Define keypad layout and movement rules
const std::map<char, std::pair<int, int>> moves = {
    {'U', {0, -1}},
    {'D', {0, 1}},
    {'L', {-1, 0}},
    {'R', {1, 0}}
};

const std::vector<std::vector<char>> keypad = {
    {'1', '2', '3'},
    {'4', '5', '6'},
    {'7', '8', '9'}
};

// Function to get the next position based on the current position and move
std::pair<int, int> getNextPosition(int x, int y, char move) {
    auto it = moves.find(move);
    if (it == moves.end()) {
        std::cerr << "Invalid move: " << move << std::endl;
        return {x, y}; // Return current position if the move is invalid
    }

    int newX = x + it->second.first;
    int newY = y + it->second.second;

    // Check if the new position is within the bounds of the keypad
    if (newX >= 0 && newX < 3 && newY >= 0 && newY < 3) {
        return {newX, newY};
    }
    return {x, y}; // If out of bounds, stay at the current position
}

int main() {
    std::ifstream file("input_level_2.txt");
    if (!file.is_open()) {
        std::cerr << "Unable to open file" << std::endl;
        return EXIT_FAILURE;
    }

    // Initial position is on '5'
    int x = 1, y = 1;
    std::string code;

    std::string line;
    while (std::getline(file, line)) {
        for (char move : line) {
            auto [newX, newY] = getNextPosition(x, y, move);
            x = newX;
            y = newY;
        }
        code += keypad[y][x];
    }

    file.close();

    std::cout << "The bathroom code is " << code << "." << std::endl;
    return EXIT_SUCCESS;
}
