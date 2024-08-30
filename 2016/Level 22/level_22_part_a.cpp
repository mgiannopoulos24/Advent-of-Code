#include <iostream>
#include <fstream>
#include <sstream>
#include <regex>
#include <vector>
#include <string>

struct Node {
    int used;
    int avail;
};

std::vector<Node> parse_data(const std::string& filename) {
    std::vector<Node> node_info;
    std::ifstream file(filename);
    if (!file.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return node_info;
    }

    std::string line;
    std::regex pattern(R"(/dev/grid/node-x\d+-y\d+\s+(\d+)T\s+(\d+)T\s+(\d+)T\s*)");
    std::smatch matches;

    while (std::getline(file, line)) {
        if (std::regex_search(line, matches, pattern)) {
            int size = std::stoi(matches[1].str());
            int used = std::stoi(matches[2].str());
            int avail = std::stoi(matches[3].str());
            node_info.push_back(Node{used, avail});
        }
    }

    return node_info;
}

int count_viable_pairs(const std::vector<Node>& node_info) {
    int count = 0;
    for (size_t i = 0; i < node_info.size(); ++i) {
        int used_i = node_info[i].used;
        if (used_i == 0) continue;

        for (size_t j = 0; j < node_info.size(); ++j) {
            if (i == j) continue;
            int avail_j = node_info[j].avail;
            if (used_i <= avail_j) {
                ++count;
            }
        }
    }
    return count;
}

int main() {
    // Filename of the input file
    std::string filename = "input_level_22.txt";

    // Parse the data from the file
    std::vector<Node> nodes = parse_data(filename);

    // Count the viable pairs
    int viable_pairs_count = count_viable_pairs(nodes);

    // Print the result
    std::cout << "Number of viable pairs: " << viable_pairs_count << std::endl;

    return 0;
}
