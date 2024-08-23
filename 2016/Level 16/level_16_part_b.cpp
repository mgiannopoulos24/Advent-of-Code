#include <iostream>
#include <string>
#include <algorithm>

// Function to generate the modified dragon curve data
std::string generateDragonCurveData(const std::string &input, int length) {
    std::string data = input;

    while (data.size() < length) {
        std::string b = data;
        std::reverse(b.begin(), b.end());
        for (char &c : b) {
            c = (c == '0') ? '1' : '0';
        }
        data = data + "0" + b;
    }

    return data.substr(0, length);
}

// Function to calculate the checksum of the data
std::string calculateChecksum(std::string data) {
    while (data.size() % 2 == 0) {
        std::string checksum;
        for (size_t i = 0; i < data.size(); i += 2) {
            if (data[i] == data[i + 1]) {
                checksum += '1';
            } else {
                checksum += '0';
            }
        }
        data = checksum;
    }

    return data;
}

int main() {
    std::string initialState = "10001110011110000"; // Change this to your input
    int diskLength = 35651584;

    std::string dragonData = generateDragonCurveData(initialState, diskLength);
    std::string checksum = calculateChecksum(dragonData);

    std::cout << "The correct checksum is: " << checksum << std::endl;

    return 0;
}
