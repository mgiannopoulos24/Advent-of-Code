#include <iostream>
#include <vector>
#include <queue>
#include <climits>
#include <map>
#include <algorithm>
#include <fstream>
#include <numeric>
#include <string>

using namespace std;

// Directions for moving in the grid
const vector<pair<int, int>> directions = {{0, 1}, {1, 0}, {0, -1}, {-1, 0}};

// Function to read the map from a file
void readMap(const string &filename, vector<string> &mapData, map<int, pair<int, int>> &locations) {
    ifstream file(filename);
    if (!file) {
        cerr << "Unable to open file " << filename << endl;
        exit(1);
    }
    string line;
    int y = 0;
    while (std::getline(file, line)) {
        mapData.push_back(line);
        for (int x = 0; x < line.length(); ++x) {
            char c = line[x];
            if (isdigit(c)) {
                locations[c - '0'] = std::make_pair(x, y);
            }
        }
        ++y;
    }
    file.close();
}

// Function to perform BFS and calculate shortest paths from a start point
map<pair<int, int>, int> bfs(const pair<int, int> &start, const vector<string> &map) {
    std::map<std::pair<int, int>, int> distances;
    queue<pair<int, int>> q;
    q.push(start);
    distances[start] = 0;
    
    while (!q.empty()) {
        auto [x, y] = q.front();
        q.pop();
        int cur_dist = distances[{x, y}];
        
        for (const auto &dir : directions) {
            int nx = x + dir.first;
            int ny = y + dir.second;
            if (nx >= 0 && ny >= 0 && ny < map.size() && nx < map[ny].length() && map[ny][nx] != '#') {
                if (distances.find({nx, ny}) == distances.end()) {
                    distances[{nx, ny}] = cur_dist + 1;
                    q.push({nx, ny});
                }
            }
        }
    }
    return distances;
}

// Function to calculate all distances between key locations
map<int, map<int, int>> calculateAllDistances(const map<int, pair<int, int>> &locations, const vector<string> &map) {
    std::map<int, std::map<int, int>> all_distances;
    for (const auto &[k1, pos1] : locations) {
        auto distances_from_k1 = bfs(pos1, map);
        for (const auto &[k2, pos2] : locations) {
            if (distances_from_k1.find(pos2) != distances_from_k1.end()) {
                all_distances[k1][k2] = distances_from_k1[pos2];
            } else {
                all_distances[k1][k2] = INT_MAX;
            }
        }
    }
    return all_distances;
}

// Function to calculate the shortest path visiting all nodes and returning to the start
int tsp(const vector<int> &keys, const map<int, map<int, int>> &distances) {
    int n = keys.size();
    vector<int> perm(n - 1); // Excluding the start point 0
    iota(perm.begin(), perm.end(), 1); // Start permutation with 1 to n-1
    
    int min_cost = INT_MAX;
    
    do {
        // Path: start -> perm[0] -> perm[1] -> ... -> perm[n-2] -> start
        int cost = distances.at(0).at(keys[perm[0]]);
        for (int i = 1; i < n - 1; ++i) {
            cost += distances.at(keys[perm[i - 1]]).at(keys[perm[i]]);
        }
        cost += distances.at(keys[perm[n - 2]]).at(0); // Returning to start
        
        min_cost = min(min_cost, cost);
    } while (next_permutation(perm.begin(), perm.end()));
    
    return min_cost;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        cerr << "Usage: " << argv[0] << " <map_file.txt>" << endl;
        return 1;
    }
    
    vector<string> mapData;
    std::map<int, std::pair<int, int>> locations;
    readMap(argv[1], mapData, locations);
    
    vector<int> keys;
    for (const auto &[key, _] : locations) {
        keys.push_back(key);
    }
    
    auto distances = calculateAllDistances(locations, mapData);
    int shortest_path_length = tsp(keys, distances);
    
    cout << "The shortest path length is " << shortest_path_length << endl;
    return 0;
}
