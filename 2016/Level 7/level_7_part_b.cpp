#include <iostream>
#include <fstream>
#include <string>
#include <unordered_set>
#include <vector>

// Function to find ABA patterns in a string and add them to a set
void findABA(const std::string& s, std::unordered_set<std::string>& abas) {
    for (size_t i = 0; i < s.length() - 2; ++i) {
        if (s[i] != s[i+1] && s[i] == s[i+2]) {
            std::string aba = s.substr(i, 3);
            abas.insert(aba);
        }
    }
}

// Function to find BAB patterns in a string
std::unordered_set<std::string> findBABs(const std::string& s) {
    std::unordered_set<std::string> babs;
    for (size_t i = 0; i < s.length() - 2; ++i) {
        if (s[i] == s[i+2] && s[i] != s[i+1]) {
            std::string bab = s.substr(i, 3);
            babs.insert(bab);
        }
    }
    return babs;
}

// Function to check if an ABA pattern has a corresponding BAB pattern
bool hasCorrespondingBAB(const std::unordered_set<std::string>& abas, const std::unordered_set<std::string>& babs) {
    for (const std::string& aba : abas) {
        std::string bab = std::string(1, aba[1]) + std::string(1, aba[0]) + std::string(1, aba[1]);
        if (babs.find(bab) != babs.end()) {
            return true;
        }
    }
    return false;
}

// Function to check if an IP address supports SSL
bool supportsSSL(const std::string& ip) {
    std::string supernet;
    std::string hypernet;
    bool inHypernet = false;
    
    std::unordered_set<std::string> abas;
    std::unordered_set<std::string> babs;
    
    // Parse the IP address
    for (char c : ip) {
        if (c == '[') {
            if (!supernet.empty()) {
                findABA(supernet, abas);
                supernet.clear();
            }
            inHypernet = true;
        } else if (c == ']') {
            if (!supernet.empty()) {
                std::unordered_set<std::string> currentBabs = findBABs(supernet);
                babs.insert(currentBabs.begin(), currentBabs.end());
                supernet.clear();
            }
            inHypernet = false;
        } else {
            supernet += c;
        }
    }
    if (!supernet.empty()) {
        if (inHypernet) {
            std::unordered_set<std::string> currentBabs = findBABs(supernet);
            babs.insert(currentBabs.begin(), currentBabs.end());
        } else {
            findABA(supernet, abas);
        }
    }
    
    return hasCorrespondingBAB(abas, babs);
}

int main() {
    std::ifstream file("input_level_7.txt");
    std::string line;
    int sslCount = 0;
    
    while (std::getline(file, line)) {
        if (supportsSSL(line)) {
            ++sslCount;
        }
    }
    
    std::cout << "Number of IPs that support SSL: " << sslCount << std::endl;
    
    return 0;
}
