#include <iostream>
#include <vector>
#include <string>
#include <openssl/evp.h>  // Requires OpenSSL library
#include <sstream>
#include <iomanip>
#include <unordered_map>

// Function to compute MD5 hash of a given string using EVP API
std::string md5(const std::string &input) {
    unsigned char digest[EVP_MAX_MD_SIZE];
    unsigned int digest_len;
    EVP_MD_CTX *ctx = EVP_MD_CTX_new();

    EVP_DigestInit_ex(ctx, EVP_md5(), NULL);
    EVP_DigestUpdate(ctx, input.c_str(), input.size());
    EVP_DigestFinal_ex(ctx, digest, &digest_len);
    EVP_MD_CTX_free(ctx);

    std::stringstream ss;
    for (unsigned int i = 0; i < digest_len; ++i) {
        ss << std::hex << std::setw(2) << std::setfill('0') << (int)digest[i];
    }

    return ss.str();
}

// Function to find the first triplet character in a hash
char find_triplet(const std::string &hash) {
    for (size_t i = 0; i < hash.size() - 2; ++i) {
        if (hash[i] == hash[i + 1] && hash[i] == hash[i + 2]) {
            return hash[i];
        }
    }
    return '\0';
}

// Function to check if a quintuplet exists in the next 1000 hashes
bool has_quintuplet(const std::unordered_map<int, std::string> &hashes, char triplet_char, int current_index) {
    std::string quintuplet(5, triplet_char);
    for (int i = current_index + 1; i <= current_index + 1000; ++i) {
        if (hashes.at(i).find(quintuplet) != std::string::npos) {
            return true;
        }
    }
    return false;
}

int main() {
    std::string salt = "cuanljph"; // Change this to your input
    int index = 0;
    int keys_found = 0;
    const int target_keys = 64;
    
    std::unordered_map<int, std::string> hashes;
    
    while (keys_found < target_keys) {
        // Generate hash for the current index if not already generated
        if (hashes.find(index) == hashes.end()) {
            hashes[index] = md5(salt + std::to_string(index));
        }

        // Check for a triplet in the current hash
        char triplet_char = find_triplet(hashes[index]);
        if (triplet_char != '\0') {
            // Precompute the next 1000 hashes if not already computed
            for (int i = index + 1; i <= index + 1000; ++i) {
                if (hashes.find(i) == hashes.end()) {
                    hashes[i] = md5(salt + std::to_string(i));
                }
            }

            // Check for a quintuplet in the next 1000 hashes
            if (has_quintuplet(hashes, triplet_char, index)) {
                keys_found++;
                std::cout << "Key " << keys_found << " found at index: " << index << std::endl;
            }
        }
        
        index++;
    }
    
    std::cout << "The index of the 64th key is: " << index - 1 << std::endl;
    
    return 0;
}
