#include <iostream>
#include <fstream>
#include <string>
#include <regex>
#include <vector>

// Function to check if a string contains an ABBA pattern
bool containsABBA(const std::string& s) {
    for (size_t i = 0; i < s.length() - 3; ++i) {
        if (s[i] != s[i+1] && s[i] == s[i+3] && s[i+1] == s[i+2]) {
            return true;
        }
    }
    return false;
}

// Function to check if an IP address supports TLS
bool supportsTLS(const std::string& ip) {
    std::string hypernet;
    std::string supernet;
    bool inHypernet = false;
    
    std::vector<std::string> hypernets;
    std::vector<std::string> supernets;
    
    // Parse the IP address
    for (char c : ip) {
        if (c == '[') {
            if (!supernet.empty()) {
                supernets.push_back(supernet);
                supernet.clear();
            }
            inHypernet = true;
        } else if (c == ']') {
            if (!supernet.empty()) {
                hypernets.push_back(supernet);
                supernet.clear();
            }
            inHypernet = false;
        } else {
            supernet += c;
        }
    }
    if (!supernet.empty()) {
        if (inHypernet) {
            hypernets.push_back(supernet);
        } else {
            supernets.push_back(supernet);
        }
    }
    
    // Check ABBA in supernets and hypernets
    bool hasABBAOutside = false;
    bool hasABBAInside = false;
    
    for (const std::string& s : supernets) {
        if (containsABBA(s)) {
            hasABBAOutside = true;
        }
    }
    
    for (const std::string& s : hypernets) {
        if (containsABBA(s)) {
            hasABBAInside = true;
        }
    }
    
    return hasABBAOutside && !hasABBAInside;
}

int main() {
    std::ifstream file("input_level_7.txt");
    std::string line;
    int tlsCount = 0;
    
    while (std::getline(file, line)) {
        if (supportsTLS(line)) {
            ++tlsCount;
        }
    }
    
    std::cout << "Number of IPs that support TLS: " << tlsCount << std::endl;
    
    return 0;
}
