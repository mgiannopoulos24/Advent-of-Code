#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <map>
#include <set>
#include <algorithm>
#include <queue>
#include <tuple>

using namespace std;

// Helper function to split a string by spaces
vector<string> split(const string &str) {
    vector<string> tokens;
    istringstream iss(str);
    string token;
    while (iss >> token) {
        tokens.push_back(token);
    }
    return tokens;
}

int main() {
    ifstream file("input_level_10.txt");
    string line;
    map<int, vector<int>> bot_chips;
    map<int, tuple<string, int, string, int>> instructions;
    map<int, int> output_bins;
    queue<int> ready_bots;
    int responsible_bot = -1;

    // Read the input file and parse the instructions
    while (getline(file, line)) {
        vector<string> parts = split(line);
        if (parts[0] == "value") {
            // value X goes to bot Y
            int value = stoi(parts[1]);
            int bot = stoi(parts[5]);
            bot_chips[bot].push_back(value);
            if (bot_chips[bot].size() == 2) {
                ready_bots.push(bot);
            }
        } else if (parts[0] == "bot") {
            // bot X gives low to {bot/output} Y and high to {bot/output} Z
            int bot = stoi(parts[1]);
            string low_target = parts[5];
            int low_index = stoi(parts[6]);
            string high_target = parts[10];
            int high_index = stoi(parts[11]);
            instructions[bot] = make_tuple(low_target, low_index, high_target, high_index);
        }
    }

    // Process bots until no more bots are ready
    while (!ready_bots.empty()) {
        int bot = ready_bots.front();
        ready_bots.pop();

        vector<int> &chips = bot_chips[bot];
        sort(chips.begin(), chips.end());
        int low_chip = chips[0];
        int high_chip = chips[1];

        if (low_chip == 17 && high_chip == 61) {
            responsible_bot = bot;
        }

        auto [low_target, low_index, high_target, high_index] = instructions[bot];

        // Give low chip
        if (low_target == "bot") {
            bot_chips[low_index].push_back(low_chip);
            if (bot_chips[low_index].size() == 2) {
                ready_bots.push(low_index);
            }
        } else {
            output_bins[low_index] = low_chip;
        }

        // Give high chip
        if (high_target == "bot") {
            bot_chips[high_index].push_back(high_chip);
            if (bot_chips[high_index].size() == 2) {
                ready_bots.push(high_index);
            }
        } else {
            output_bins[high_index] = high_chip;
        }

        // Clear chips from the current bot
        chips.clear();
    }

    // Calculate and output the result for Part Two
    int product = output_bins[0] * output_bins[1] * output_bins[2];
    cout << "Product of outputs 0, 1, and 2: " << product << endl;

    return 0;
}
