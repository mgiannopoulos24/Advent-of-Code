#include <iostream>
#include <string>
#include <queue>
#include <openssl/evp.h>
#include <openssl/md5.h>

// Function to generate the MD5 hash using the EVP interface
std::string md5(const std::string& input) {
    EVP_MD_CTX* context = EVP_MD_CTX_new();
    const EVP_MD* md = EVP_md5();
    unsigned char hash[EVP_MAX_MD_SIZE];
    unsigned int lengthOfHash = 0;

    EVP_DigestInit_ex(context, md, NULL);
    EVP_DigestUpdate(context, input.c_str(), input.size());
    EVP_DigestFinal_ex(context, hash, &lengthOfHash);
    EVP_MD_CTX_free(context);

    std::string mdString;
    char buffer[3];
    for (unsigned int i = 0; i < lengthOfHash; ++i) {
        snprintf(buffer, sizeof(buffer), "%02x", hash[i]);
        mdString.append(buffer);
    }

    return mdString;
}

// Check if a character in the hash represents an open door
bool isOpen(char c) {
    return c >= 'b' && c <= 'f';
}

// Define the structure for the state in BFS
struct State {
    int x, y;
    std::string path;
};

// Function to find the shortest path using BFS
std::string findShortestPath(const std::string& passcode) {
    std::queue<State> q;
    q.push({0, 0, ""});

    const int targetX = 3, targetY = 3;
    const std::vector<std::pair<int, int>> directions = {{0, -1}, {0, 1}, {-1, 0}, {1, 0}};
    const std::string directionChars = "UDLR";

    while (!q.empty()) {
        State current = q.front();
        q.pop();

        // If we reached the target, return the path
        if (current.x == targetX && current.y == targetY) {
            return current.path;
        }

        std::string hash = md5(passcode + current.path);
        for (int i = 0; i < 4; ++i) {
            if (isOpen(hash[i])) {
                int newX = current.x + directions[i].first;
                int newY = current.y + directions[i].second;

                if (newX >= 0 && newX <= 3 && newY >= 0 && newY <= 3) {
                    q.push({newX, newY, current.path + directionChars[i]});
                }
            }
        }
    }

    return ""; // In case there is no valid path
}

int main() {
    std::string passcode = "ioramepc"; // Change this to your input
    std::string shortestPath = findShortestPath(passcode);

    std::cout << "The shortest path is: " << shortestPath << std::endl;
    return 0;
}
