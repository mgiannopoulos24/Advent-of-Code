#include <iostream>
#include <fstream>
#include <sstream>
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

    int validTriangles = 0;
    std::string line;

    while (std::getline(inputFile, line)) {
        std::istringstream ss(line);
        int a, b, c;
        ss >> a >> b >> c;

        if (isValidTriangle(a, b, c)) {
            validTriangles++;
        }
    }

    inputFile.close();

    std::cout << "Number of valid triangles: " << validTriangles << std::endl;

    return 0;
}
