#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>

// Function to parse the input file and get the disc configurations
std::vector<std::pair<int, int>> parseInput(const std::string &filename) {
    std::ifstream file(filename);
    std::string line;
    std::vector<std::pair<int, int>> discs;

    while (std::getline(file, line)) {
        int discNumber, positions, initialPosition;
        std::sscanf(line.c_str(), "Disc #%d has %d positions; at time=0, it is at position %d.", &discNumber, &positions, &initialPosition);
        discs.push_back({positions, initialPosition});
    }

    return discs;
}

// Function to check if all discs align at a given time
bool checkAlignment(const std::vector<std::pair<int, int>> &discs, int time) {
    for (int i = 0; i < discs.size(); ++i) {
        int positions = discs[i].first;
        int initialPosition = discs[i].second;
        if ((initialPosition + time + i + 1) % positions != 0) {
            return false;
        }
    }
    return true;
}

int main() {
    // Read the disc configurations from the input file
    std::string filename = "input_level_15.txt";
    std::vector<std::pair<int, int>> discs = parseInput(filename);

    // Find the first time when all discs align
    int time = 0;
    while (!checkAlignment(discs, time)) {
        ++time;
    }

    std::cout << "The first time you can press the button to get a capsule is: " << time << std::endl;
    return 0;
}
