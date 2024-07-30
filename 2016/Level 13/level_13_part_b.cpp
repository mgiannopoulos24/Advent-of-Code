#include <iostream>
#include <queue>
#include <unordered_set>
#include <utility>

// Define the favorite number given in the puzzle
const int FAVORITE_NUMBER = 1358; // Change this to your input

// Function to determine if a cell is a wall or an open space
bool isWall(int x, int y) {
    int formula = x*x + 3*x + 2*x*y + y + y*y + FAVORITE_NUMBER;
    int bitCount = __builtin_popcount(formula);
    return bitCount % 2 != 0;
}

// BFS to find the number of reachable locations within 50 steps
int bfs() {
    std::queue<std::pair<int, int>> q;
    std::unordered_set<long long> visited;

    // Start from (1, 1)
    q.push({1, 1});
    visited.insert(1LL << 32 | 1);

    int steps = 0;
    int reachableLocations = 1; // Including the starting location
    int directions[4][2] = {{0, 1}, {1, 0}, {0, -1}, {-1, 0}};
    
    while (!q.empty() && steps < 50) {
        int levelSize = q.size();
        for (int i = 0; i < levelSize; ++i) {
            auto [x, y] = q.front();
            q.pop();

            // Explore neighbors
            for (auto &dir : directions) {
                int newX = x + dir[0];
                int newY = y + dir[1];

                // Ensure we stay within bounds and avoid walls
                if (newX >= 0 && newY >= 0 && !isWall(newX, newY)) {
                    long long key = (long long)newX << 32 | newY;
                    if (visited.find(key) == visited.end()) {
                        visited.insert(key);
                        q.push({newX, newY});
                        ++reachableLocations;
                    }
                }
            }
        }
        ++steps;
    }

    return reachableLocations;
}

int main() {
    int reachableLocations = bfs();

    std::cout << "Number of distinct locations reachable within 50 steps is " << reachableLocations << std::endl;

    return 0;
}
