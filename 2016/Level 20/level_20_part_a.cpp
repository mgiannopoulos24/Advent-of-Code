#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>
#include <algorithm>

struct Range {
    unsigned int start, end;
};

std::vector<Range> readRanges(const std::string &filename) {
    std::ifstream infile(filename);
    std::string line;
    std::vector<Range> ranges;
    
    while (std::getline(infile, line)) {
        std::stringstream ss(line);
        unsigned int start, end;
        char dash;
        ss >> start >> dash >> end;
        ranges.push_back({start, end});
    }
    
    return ranges;
}

unsigned int findLowestUnblockedIP(std::vector<Range> &ranges) {
    std::sort(ranges.begin(), ranges.end(), [](const Range &a, const Range &b) {
        return a.start < b.start;
    });

    unsigned int current = 0;
    
    for (const auto &range : ranges) {
        if (current < range.start) {
            return current;
        }
        if (current <= range.end) {
            current = range.end + 1;
        }
    }
    
    return current;
}

int main() {
    std::vector<Range> ranges = readRanges("input_level_20.txt");
    unsigned int lowestUnblockedIP = findLowestUnblockedIP(ranges);
    
    std::cout << "The lowest-valued IP that is not blocked is: " << lowestUnblockedIP << std::endl;
    
    return 0;
}
