#include <iostream>
#include <fstream>
#include <vector>
#include <unordered_map>
#include <string>
#include <limits>

using namespace std;

int main() {
    ifstream infile("input_level_6.txt");
    
    if (!infile) {
        cerr << "Unable to open file input_level_6.txt";
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
    
    string originalMessage;
    
    // Determine the least frequent character for each column
    for (const auto& freqMap : frequencyMaps) {
        char leastFrequentChar;
        int minCount = numeric_limits<int>::max();
        
        for (const auto& entry : freqMap) {
            if (entry.second < minCount) {
                minCount = entry.second;
                leastFrequentChar = entry.first;
            }
        }
        
        originalMessage += leastFrequentChar;
    }
    
    cout << "The original message is: " << originalMessage << endl;
    
    return 0;
}
