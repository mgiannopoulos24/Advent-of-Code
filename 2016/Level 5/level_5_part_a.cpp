#include <iostream>
#include <openssl/evp.h>
#include <iomanip>
#include <sstream>

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
    std::string password;
    int index = 0;

    while (password.size() < 8) {
        std::string toHash = doorID + std::to_string(index);
        std::string hash = md5(toHash);

        if (hash.substr(0, 5) == "00000") {
            password += hash[5];
        }

        ++index;
    }

    return password;
}

int main() {
    std::string doorID = "ugkcyxxp"; // Your input here
    std::string password = findPassword(doorID);

    std::cout << "The password is: " << password << std::endl;

    return 0;
}
