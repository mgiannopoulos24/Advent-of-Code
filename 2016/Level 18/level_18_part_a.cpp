#include <iostream>
#include <fstream>
#include <string>
#include <vector>

// Function to determine if a tile is safe or a trap based on the previous row
char determineTile(const std::string& previousRow, int index) {
    char left = (index == 0) ? '.' : previousRow[index - 1];
    char center = previousRow[index];
    char right = (index == previousRow.length() - 1) ? '.' : previousRow[index + 1];

    if ((left == '^' && center == '^' && right == '.') ||
        (left == '.' && center == '^' && right == '^') ||
        (left == '^' && center == '.' && right == '.') ||
        (left == '.' && center == '.' && right == '^')) {
        return '^';
    } else {
        return '.';
    }
}

int main() {
    std::ifstream inputFile("input_level_18.txt");
    if (!inputFile) {
        std::cerr << "Unable to open file input.txt";
        return 1;
    }

    std::string firstRow;
    std::getline(inputFile, firstRow);
    inputFile.close();

    int numRows = 40; // Change this to 400000 for Part Two
    std::vector<std::string> rows;
    rows.push_back(firstRow);

    int safeTiles = 0;

    // Count safe tiles in the first row
    for (char tile : firstRow) {
        if (tile == '.') {
            safeTiles++;
        }
    }

    // Generate subsequent rows
    for (int i = 1; i < numRows; i++) {
        std::string previousRow = rows.back();
        std::string newRow;
        for (size_t j = 0; j < previousRow.length(); j++) {
            char newTile = determineTile(previousRow, j);
            newRow += newTile;
            if (newTile == '.') {
                safeTiles++;
            }
        }
        rows.push_back(newRow);
    }

    std::cout << "Number of safe tiles: " << safeTiles << std::endl;

    return 0;
}
