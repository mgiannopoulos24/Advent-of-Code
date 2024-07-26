#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>
#include <map>
#include <algorithm>

struct Room {
    std::string name;
    int sectorID;
    std::string checksum;
};

bool isRealRoom(const Room& room) {
    std::map<char, int> letterCount;
    for (char c : room.name) {
        if (c != '-') {
            letterCount[c]++;
        }
    }

    std::vector<std::pair<char, int>> sortedLetters(letterCount.begin(), letterCount.end());
    std::sort(sortedLetters.begin(), sortedLetters.end(), [](const std::pair<char, int>& a, const std::pair<char, int>& b) {
        if (a.second == b.second) {
            return a.first < b.first;
        }
        return a.second > b.second;
    });

    std::string calculatedChecksum;
    for (int i = 0; i < 5 && i < sortedLetters.size(); ++i) {
        calculatedChecksum += sortedLetters[i].first;
    }

    return calculatedChecksum == room.checksum;
}

std::string decryptName(const std::string& encryptedName, int sectorID) {
    std::string decryptedName;
    for (char c : encryptedName) {
        if (c == '-') {
            decryptedName += ' ';
        } else {
            decryptedName += (c - 'a' + sectorID) % 26 + 'a';
        }
    }
    return decryptedName;
}

int main() {
    std::ifstream inputFile("input_level_4.txt");
    if (!inputFile) {
        std::cerr << "Error opening file!" << std::endl;
        return 1;
    }

    std::vector<std::string> input;
    std::string line;
    while (std::getline(inputFile, line)) {
        input.push_back(line);
    }

    int sumOfSectorIDs = 0;
    int northPoleSectorID = -1;

    for (const std::string& line : input) {
        std::size_t lastDash = line.find_last_of('-');
        std::size_t openBracket = line.find('[');
        std::size_t closeBracket = line.find(']');

        std::string name = line.substr(0, lastDash);
        int sectorID = std::stoi(line.substr(lastDash + 1, openBracket - lastDash - 1));
        std::string checksum = line.substr(openBracket + 1, closeBracket - openBracket - 1);

        Room room = {name, sectorID, checksum};

        if (isRealRoom(room)) {
            sumOfSectorIDs += room.sectorID;
            std::string decryptedName = decryptName(room.name, room.sectorID);
            if (decryptedName.find("northpole object storage") != std::string::npos) {
                northPoleSectorID = room.sectorID;
            }
        }
    }

    std::cout << "Sum of sector IDs of real rooms: " << sumOfSectorIDs << std::endl;
    if (northPoleSectorID != -1) {
        std::cout << "Sector ID of the room where North Pole objects are stored: " << northPoleSectorID << std::endl;
    } else {
        std::cout << "North Pole object storage room not found." << std::endl;
    }

    return 0;
}
