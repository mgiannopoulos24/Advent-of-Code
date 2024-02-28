#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

bool has_repeating_pair(const char *str) {
    for (int i = 0; str[i] != '\0' && str[i+1] != '\0'; i++) {
        for (int j = i + 2; str[j] != '\0' && str[j+1] != '\0'; j++) {
            if (str[i] == str[j] && str[i+1] == str[j+1]) {
                return true;
            }
        }
    }
    return false;
}

bool has_repeating_letter_with_one_between(const char *str) {
    for (int i = 0; str[i] != '\0' && str[i+2] != '\0'; i++) {
        if (str[i] == str[i+2]) {
            return true;
        }
    }
    return false;
}

bool is_nice(const char *str) {
    return has_repeating_pair(str) && has_repeating_letter_with_one_between(str);
}

int main() {
    FILE *file = fopen("input_level_5.txt", "r");
    if (!file) {
        printf("Error opening file\n");
        return 1;
    }

    char line[256];
    int nice_count = 0;
    while (fgets(line, sizeof(line), file)) {
        // Remove newline character, if present
        line[strcspn(line, "\n")] = 0;
        if (is_nice(line)) nice_count++;
    }

    fclose(file);

    printf("Number of nice strings: %d\n", nice_count);

    return 0;
}
