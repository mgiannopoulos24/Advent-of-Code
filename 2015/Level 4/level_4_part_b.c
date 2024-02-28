#include <stdio.h>
#include <string.h>
#include <openssl/md5.h>

void to_hex_string(unsigned char *hash, char *hex_string) {
    for (int i = 0; i < MD5_DIGEST_LENGTH; i++) {
        sprintf(hex_string + (i * 2), "%02x", hash[i]);
    }
    hex_string[MD5_DIGEST_LENGTH * 2] = '\0';
}

int main() {
    char *secret_key = "bgvyzdsv";
    unsigned char hash[MD5_DIGEST_LENGTH];
    char hex_string[MD5_DIGEST_LENGTH * 2 + 1];
    int number = 1;
    char input[256];

    while (1) {
        // Prepare input: secret key followed by number
        sprintf(input, "%s%d", secret_key, number);
        
        // Calculate MD5 hash
        MD5((unsigned char*)input, strlen(input), hash);
        
        // Convert hash to hex string
        to_hex_string(hash, hex_string);

        // Check if the hex string starts with six zeroes
        if (strncmp(hex_string, "000000", 6) == 0) {
            printf("The lowest positive number is: %d\n", number);
            printf("Hash: %s\n", hex_string);
            break;
        }
        
        number++;
    }

    return 0;
}
