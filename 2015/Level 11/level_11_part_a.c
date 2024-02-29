#include <stdio.h>
#include <string.h>

void incrementPassword(char* password) {
    int length = strlen(password);
    for (int i = length - 1; i >= 0; i--) {
        if (password[i] == 'z') {
            password[i] = 'a';
        } else {
            password[i]++;
            if (password[i] == 'i' || password[i] == 'o' || password[i] == 'l') {
                password[i]++; // Skip forbidden letters
            }
            break;
        }
    }
}

int hasStraight(const char* password) {
    for (int i = 0; password[i + 2] != '\0'; i++) {
        if (password[i] + 1 == password[i + 1] && password[i] + 2 == password[i + 2]) {
            return 1;
        }
    }
    return 0;
}

int hasForbiddenLetters(const char* password) {
    return strpbrk(password, "iol") != NULL;
}

int hasTwoPairs(const char* password) {
    int pairs = 0;
    for (int i = 0; password[i + 1] != '\0'; i++) {
        if (password[i] == password[i + 1]) {
            pairs++;
            i++; // Skip the next letter to avoid overlapping
        }
    }
    return pairs >= 2;
}

int isValidPassword(const char* password) {
    return hasStraight(password) && !hasForbiddenLetters(password) && hasTwoPairs(password);
}

int main() {
    char password[] = "hxbxwxba"; // Starting password

    do {
        incrementPassword(password);
    } while (!isValidPassword(password));

    printf("Next valid password: %s\n", password);
    return 0;
}
