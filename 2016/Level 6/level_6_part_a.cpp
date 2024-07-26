#include <iostream>
#include <fstream>
#include <vector>
#include <unordered_map>
#include <algorithm>
#include <string>

using namespace std;

int main() {
    ifstream infile("input_level_6.txt");
    
    if (!infile) {
        cerr << "Unable to open file input.txt";
        return 1;
    }
    
    vector<string> messages;
    string line;
    
    // Read all lines from the input file
    while (getline(infile, line)) {
        messages.push_back(line);
    }
    
    infile.close();
    
    if (messages.empty()) {
        cerr << "No data found in the file.";
        return 1;
    }
    
    int numMessages = messages.size();
    int messageLength = messages[0].length();
    
    // Vector of unordered_maps to store frequency counts for each column
    vector<unordered_map<char, int>> frequencyMaps(messageLength);
    
    // Populate frequency maps
    for (const auto& msg : messages) {
        for (int i = 0; i < messageLength; ++i) {
            frequencyMaps[i][msg[i]]++;
        }
    }
    
    string errorCorrectedMessage;
    
    // Determine the most frequent character for each column
    for (const auto& freqMap : frequencyMaps) {
        char mostFrequentChar;
        int maxCount = 0;
        
        for (const auto& entry : freqMap) {
            if (entry.second > maxCount) {
                maxCount = entry.second;
                mostFrequentChar = entry.first;
            }
        }
        
        errorCorrectedMessage += mostFrequentChar;
    }
    
    cout << "The error-corrected message is: " << errorCorrectedMessage << endl;
    
    return 0;
}
