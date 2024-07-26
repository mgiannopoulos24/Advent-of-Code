#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <unordered_map>

using namespace std;

// Define the 5x6 pixel representation for each letter
const vector<string> font = {
    "#####..#...#..#...#..#...#..#...#..#####", // A
    "####..#...#..####..#...#..####..#####",    // B
    "####..#.....#.....#.....#..####",           // C
    "####..#.....#.....#.....#..####",           // D
    "#####..#.....#####..#.....#####",            // E
    "#####..#.....#####..#.....#....",            // F
    "####..#.....#.....#..###..####",            // G
    "#...#..#...#..####..#...#..#...#",           // H
    "###...#...#...#..###",                       // I
    "###..#...#..#...#..###",                     // J
    "#...#..#..##..#..#..#...#",                  // K
    "#...#..#..#..#..####",                       // L
    "#...###..#.#..#.#..#...#",                   // M
    "#...##..#..#..#..#...#",                     // N
    "###..#..#..#..###",                         // O
    "####..#..###..#....#",                       // P
    "###..#..#..###..#..#",                       // Q
    "####..#..###..#..#..###",                   // R
    "###..#..###..#..###",                       // S
    "####..#..#..#...#",                         // T
    "#...#..#...#..###",                         // U
    "#...#..###..#..#",                          // V
    "#...#..#..###..#..#",                       // W
    "#...#..#..#..#..#",                         // X
    "#...#..###..#..#",                          // Y
    "####..#..###..####"                         // Z
};

// Function to initialize the screen
void initializeScreen(vector<vector<int>>& screen) {
    for (auto& row : screen) {
        fill(row.begin(), row.end(), 0);
    }
}

// Function to display the screen
void displayScreen(const vector<vector<int>>& screen) {
    for (const auto& row : screen) {
        for (int pixel : row) {
            cout << (pixel ? '#' : '.');
        }
        cout << endl;
    }
}

// Function to count the number of lit pixels
int countLitPixels(const vector<vector<int>>& screen) {
    int count = 0;
    for (const auto& row : screen) {
        for (int pixel : row) {
            if (pixel) count++;
        }
    }
    return count;
}

// Function to execute the instructions
void executeInstructions(vector<vector<int>>& screen, const vector<string>& instructions) {
    for (const string& instruction : instructions) {
        if (instruction.find("rect") == 0) {
            // rect AxB
            int A, B;
            sscanf(instruction.c_str(), "rect %dx%d", &A, &B);
            for (int i = 0; i < B; ++i) {
                for (int j = 0; j < A; ++j) {
                    screen[i][j] = 1;
                }
            }
        } else if (instruction.find("rotate row") == 0) {
            // rotate row y=A by B
            int A, B;
            sscanf(instruction.c_str(), "rotate row y=%d by %d", &A, &B);
            vector<int> tempRow(screen[0].size());
            for (int i = 0; i < screen[0].size(); ++i) {
                tempRow[(i + B) % screen[0].size()] = screen[A][i];
            }
            screen[A] = tempRow;
        } else if (instruction.find("rotate column") == 0) {
            // rotate column x=A by B
            int A, B;
            sscanf(instruction.c_str(), "rotate column x=%d by %d", &A, &B);
            vector<int> tempCol(screen.size());
            for (int i = 0; i < screen.size(); ++i) {
                tempCol[(i + B) % screen.size()] = screen[i][A];
            }
            for (int i = 0; i < screen.size(); ++i) {
                screen[i][A] = tempCol[i];
            }
        }
    }
}

// Function to convert a 5x6 block to a character
char getCharacterFromPixels(const vector<vector<int>>& screen, int startX) {
    string pixelBlock;
    for (int i = 0; i < 6; ++i) {
        for (int j = startX; j < startX + 5; ++j) {
            pixelBlock += (screen[i][j] ? '#' : '.');
        }
    }

    // Check each letter in the font mapping
    for (int i = 0; i < 26; ++i) {
        if (pixelBlock == font[i]) {
            return 'A' + i;
        }
    }
    return '?';
}

int main() {
    // Define screen size
    const int width = 50;
    const int height = 6;

    // Initialize the screen
    vector<vector<int>> screen(height, vector<int>(width, 0));

    // Read instructions from the file
    ifstream inputFile("input_level_8.txt");
    vector<string> instructions;
    string line;
    while (getline(inputFile, line)) {
        instructions.push_back(line);
    }
    inputFile.close();

    // Execute the instructions
    executeInstructions(screen, instructions);

    // Display the screen (for debugging purposes)
    displayScreen(screen);

    // Decode and display the message
    string message;
    for (int i = 0; i < width; i += 5) {
        message += getCharacterFromPixels(screen, i);
    }

    cout << "Decoded message: " << message << endl;

    return 0;
}
