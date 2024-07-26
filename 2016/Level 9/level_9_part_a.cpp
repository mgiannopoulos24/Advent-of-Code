#include <iostream>
#include <fstream>
#include <string>
#include <algorithm>
#include <cctype>

using namespace std;

// Function to compute the decompressed length
long long computeDecompressedLength(const string& input) {
    long long length = 0;
    size_t i = 0;
    while (i < input.size()) {
        if (input[i] == '(') {
            // Find the closing parenthesis
            size_t closeParen = input.find(')', i);
            if (closeParen == string::npos) {
                cerr << "Error: Mismatched parentheses" << endl;
                exit(1);
            }

            // Extract marker
            string marker = input.substr(i + 1, closeParen - i - 1);
            size_t xPos = marker.find('x');
            if (xPos == string::npos) {
                cerr << "Error: Invalid marker format" << endl;
                exit(1);
            }
            
            // Extract N and M from the marker
            int lengthToRepeat = stoi(marker.substr(0, xPos));
            int repeatTimes = stoi(marker.substr(xPos + 1));

            // Move index past the marker
            i = closeParen + 1;
            
            // Compute the length of the section to be repeated
            length += lengthToRepeat * repeatTimes;
            
            // Skip over the repeated section
            i += lengthToRepeat;
        } else {
            // Count the length of characters outside markers
            length++;
            i++;
        }
    }
    
    return length;
}

int main() {
    // Read input from file
    ifstream inputFile("input_level_9.txt");
    if (!inputFile.is_open()) {
        cerr << "Error: Could not open file" << endl;
        return 1;
    }
    
    string input;
    getline(inputFile, input);
    inputFile.close();
    
    // Remove whitespace from input
    input.erase(remove_if(input.begin(), input.end(), ::isspace), input.end());

    // Compute and output the decompressed length
    long long length = computeDecompressedLength(input);
    cout << "Decompressed length: " << length << endl;

    return 0;
}
