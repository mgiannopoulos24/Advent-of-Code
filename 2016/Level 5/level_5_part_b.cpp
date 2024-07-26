#include <iostream>
#include <openssl/evp.h>
#include <iomanip>
#include <sstream>
#include <chrono>
#include <thread>

std::string md5(const std::string& input) {
    unsigned char digest[EVP_MAX_MD_SIZE];
    unsigned int digest_len;

    EVP_MD_CTX* context = EVP_MD_CTX_new();
    if (!context) {
        throw std::runtime_error("Failed to create EVP_MD_CTX");
    }

    if (1 != EVP_DigestInit_ex(context, EVP_md5(), nullptr)) {
        EVP_MD_CTX_free(context);
        throw std::runtime_error("Failed to initialize MD5 digest");
    }

    if (1 != EVP_DigestUpdate(context, input.c_str(), input.size())) {
        EVP_MD_CTX_free(context);
        throw std::runtime_error("Failed to update MD5 digest");
    }

    if (1 != EVP_DigestFinal_ex(context, digest, &digest_len)) {
        EVP_MD_CTX_free(context);
        throw std::runtime_error("Failed to finalize MD5 digest");
    }

    EVP_MD_CTX_free(context);

    std::ostringstream ss;
    for (unsigned int i = 0; i < digest_len; ++i) {
        ss << std::hex << std::setw(2) << std::setfill('0') << (int)digest[i];
    }
    return ss.str();
}

std::string findPassword(const std::string& doorID) {
    std::string password(8, '_'); // Initialize password with 8 underscores
    int filledPositions = 0; // Count of filled positions
    int index = 0;

    while (filledPositions < 8) {
        std::string toHash = doorID + std::to_string(index);
        std::string hash = md5(toHash);

        if (hash.substr(0, 5) == "00000") {
            int position = hash[5] - '0';
            if (position >= 0 && position < 8 && password[position] == '_') {
                password[position] = hash[6];
                ++filledPositions;

                // Print the cinematic "decrypting" animation
                std::cout << "\rDecrypting password: " << password << std::flush;
                std::this_thread::sleep_for(std::chrono::milliseconds(100)); // Delay for animation effect
            }
        }

        ++index;
    }

    std::cout << std::endl; // Move to the next line after animation
    return password;
}

int main() {
    std::string doorID = "ugkcyxxp"; // Your input here
    std::string password = findPassword(doorID);

    std::cout << "The password is: " << password << std::endl;

    return 0;
}
