#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>
#include <algorithm>

using namespace std;

// Helper function to parse a range string and return a pair of integers
pair<uint32_t, uint32_t> parseRange(const string &rangeStr) {
    stringstream ss(rangeStr);
    uint32_t start, end;
    char dash;
    ss >> start >> dash >> end;
    return make_pair(start, end);
}

int main() {
    // File containing the blacklist ranges
    string filename = "input_level_20.txt";
    ifstream inputFile(filename);

    // Check if file opened successfully
    if (!inputFile.is_open()) {
        cerr << "Error: Could not open file " << filename << endl;
        return 1;
    }

    vector<pair<uint32_t, uint32_t>> ranges;
    string line;

    // Read each line from the file and parse the range
    while (getline(inputFile, line)) {
        ranges.push_back(parseRange(line));
    }

    // Close the file
    inputFile.close();

    // Sort the ranges by start, then by end
    sort(ranges.begin(), ranges.end());

    // Initialize variables
    uint32_t allowed_ips = 0;
    int64_t current_end = -1;

    // Iterate over the sorted list
    for (const auto &range : ranges) {
        uint32_t start = range.first;
        uint32_t end = range.second;

        if (start > current_end + 1) {
            // Count the allowed IPs between the current_end and the start of the next range
            allowed_ips += (start - (current_end + 1));
        }
        // Update the current_end to the maximum of the current_end or the end of the current range
        current_end = max(current_end, static_cast<int64_t>(end));
    }

    // Count the allowed IPs after the last range
    if (current_end < 4294967295) {
        allowed_ips += (4294967295 - current_end);
    }

    // Output the result
    cout << "Number of allowed IPs: " << allowed_ips << endl;

    return 0;
}
