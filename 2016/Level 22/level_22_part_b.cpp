#include <iostream>
#include <fstream>
#include <sstream>
#include <regex>
#include <vector>
#include <string>
#include <cmath>
#include <queue>
#include <set>

struct Node {
    int size, used, avail;
};

// Function to parse the data from the input file
std::vector<std::vector<Node>> parse_data(const std::string& filename, int& width, int& height, int& emptyX, int& emptyY) {
    std::vector<std::vector<Node>> grid;
    std::ifstream file(filename);
    if (!file.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return grid;
    }

    std::string line;
    std::regex pattern(R"(/dev/grid/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T\s*)");
    std::smatch matches;

    int max_x = 0;
    int max_y = 0;

    // First pass: determine grid dimensions
    while (std::getline(file, line)) {
        if (std::regex_search(line, matches, pattern)) {
            int x = std::stoi(matches[1].str());
            int y = std::stoi(matches[2].str());
            if (x > max_x) max_x = x;
            if (y > max_y) max_y = y;
        }
    }

    // Set width and height
    width = max_x + 1;
    height = max_y + 1;

    // Resize grid to match dimensions
    grid.resize(height, std::vector<Node>(width));

    // Reset file read position to the beginning
    file.clear();
    file.seekg(0, std::ios::beg);

    // Second pass: populate grid and find the empty node
    while (std::getline(file, line)) {
        if (std::regex_search(line, matches, pattern)) {
            int x = std::stoi(matches[1].str());
            int y = std::stoi(matches[2].str());
            int size = std::stoi(matches[3].str());
            int used = std::stoi(matches[4].str());
            int avail = std::stoi(matches[5].str());

            grid[y][x] = {size, used, avail};

            // Identify the empty node
            if (used == 0) {
                emptyX = x;
                emptyY = y;
            }
        }
    }

    return grid;
}

// Function to calculate the Manhattan distance
int manhattan_distance(int x1, int y1, int x2, int y2) {
    return std::abs(x1 - x2) + std::abs(y1 - y2);
}

// BFS function to find the shortest path to the target node
int bfs(const std::vector<std::vector<Node>>& grid, int startX, int startY, int targetX, int targetY) {
    int height = grid.size();
    int width = grid[0].size();
    std::queue<std::tuple<int, int, int>> queue;
    std::set<std::pair<int, int>> visited;
    queue.push({startX, startY, 0});
    visited.insert({startX, startY});

    int directions[4][2] = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};

    while (!queue.empty()) {
        auto [x, y, steps] = queue.front();
        queue.pop();

        if (x == targetX && y == targetY) {
            return steps;
        }

        for (const auto& direction : directions) {
            int nx = x + direction[0];
            int ny = y + direction[1];

            if (nx >= 0 && nx < width && ny >= 0 && ny < height && visited.find({nx, ny}) == visited.end() &&
                grid[ny][nx].used <= grid[startY][startX].size) {
                visited.insert({nx, ny});
                queue.push({nx, ny, steps + 1});
            }
        }
    }

    return std::numeric_limits<int>::max(); // If no path is found
}

// Function to calculate steps to move data to node-x0-y0
int calculate_steps_to_move_data(const std::vector<std::vector<Node>>& grid, int emptyX, int emptyY, int width) {
    // Position of the goal data is at (width-1, 0), we need the empty node at (width-2, 0)
    int targetX = width - 2;
    int targetY = 0;

    // Calculate steps to move the empty node to the target position using BFS
    int steps_to_goal = bfs(grid, emptyX, emptyY, targetX, targetY);

    // Steps required to shift the goal data to the top-left corner:
    // (width - 2) * 5 shifts to move the goal left to node-x0-y0 (patterned moves)
    int steps_to_top_left = (width - 2) * 5;

    // Total steps: to position the empty node + to shift the goal data left
    return steps_to_goal + steps_to_top_left + 1;  // +1 to move the goal data into the target node
}

int main() {
    // Filename of the input file
    std::string filename = "input_level_22.txt";

    int width, height, emptyX, emptyY;
    std::vector<std::vector<Node>> grid = parse_data(filename, width, height, emptyX, emptyY);

    // Calculate the total number of steps required for Part 2
    int total_steps = calculate_steps_to_move_data(grid, emptyX, emptyY, width);

    // Print the result for Part 2
    std::cout << "Fewest number of steps required to move goal data to node-x0-y0: " << total_steps << std::endl;

    return 0;
}
