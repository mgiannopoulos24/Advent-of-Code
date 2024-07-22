#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>

bool isValidTriangle(int a, int b, int c) {
    return (a + b > c) && (a + c > b) && (b + c > a);
}

int main() {
    std::ifstream inputFile("input_level_3.txt");
    if (!inputFile) {
        std::cerr << "Unable to open file input.txt" << std::endl;
        return 1;
    }

    std::vector<std::vector<int>> data;
    std::string line;

    // Read the input into a 2D vector
    while (std::getline(inputFile, line)) {
        std::istringstream ss(line);
        int num;
        std::vector<int> row;
        while (ss >> num) {
            row.push_back(num);
        }
        data.push_back(row);
    }
    inputFile.close();

    int validTriangles = 0;

    // Process columns and check for valid triangles
    for (size_t col = 0; col < data[0].size(); ++col) {
        for (size_t row = 0; row < data.size(); row += 3) {
            if (row + 2 < data.size()) {
                int a = data[row][col];
                int b = data[row + 1][col];
                int c = data[row + 2][col];
                if (isValidTriangle(a, b, c)) {
                    validTriangles++;
                }
            }
        }
    }

    std::cout << "Number of valid triangles: " << validTriangles << std::endl;

    return 0;
}
