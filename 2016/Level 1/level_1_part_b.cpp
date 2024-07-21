#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <cstdlib>
#include <cmath>
#include <set>
#include <utility>

// Function to update the position based on direction and distance
void updatePosition(int& x, int& y, char direction, int distance) {
    switch (direction) {
        case 'N': y += distance; break;
        case 'S': y -= distance; break;
        case 'E': x += distance; break;
        case 'W': x -= distance; break;
    }
}

// Function to compute Manhattan distance
int manhattanDistance(int x1, int y1, int x2, int y2) {
    return std::abs(x2 - x1) + std::abs(y2 - y1);
}

int main() {
    std::ifstream file("input_level_1.txt");
    if (!file.is_open()) {
        std::cerr << "Unable to open file" << std::endl;
        return EXIT_FAILURE;
    }

    // Initialize starting position and direction
    int x = 0, y = 0;
    char direction = 'N';

    // Set to track visited locations
    std::set<std::pair<int, int>> visitedLocations;
    visitedLocations.insert({x, y});  // Insert starting position

    // Read the input line
    std::string line;
    std::getline(file, line);
    file.close();

    // Tokenize the input line
    std::stringstream ss(line);
    std::string token;

    while (std::getline(ss, token, ',')) {
        // Remove leading and trailing spaces
        token.erase(0, token.find_first_not_of(" "));
        token.erase(token.find_last_not_of(" ") + 1);

        char turn = token[0];
        int distance = std::stoi(token.substr(1));

        // Update direction
        if (turn == 'L') {
            switch (direction) {
                case 'N': direction = 'W'; break;
                case 'W': direction = 'S'; break;
                case 'S': direction = 'E'; break;
                case 'E': direction = 'N'; break;
            }
        } else if (turn == 'R') {
            switch (direction) {
                case 'N': direction = 'E'; break;
                case 'E': direction = 'S'; break;
                case 'S': direction = 'W'; break;
                case 'W': direction = 'N'; break;
            }
        }

        // Move in the current direction
        for (int i = 0; i < distance; ++i) {
            updatePosition(x, y, direction, 1);  // Move one block at a time
            auto currentLocation = std::make_pair(x, y);
            
            // Check if this location has been visited before
            if (visitedLocations.find(currentLocation) != visitedLocations.end()) {
                int distanceToOrigin = manhattanDistance(0, 0, x, y);
                std::cout << "The first location visited twice is " << distanceToOrigin << " blocks away." << std::endl;
                return EXIT_SUCCESS;
            }
            
            visitedLocations.insert(currentLocation);
        }
    }

    std::cerr << "No location was visited twice." << std::endl;
    return EXIT_FAILURE;
}
