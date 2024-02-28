#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

bool is_nice(const char* str) {
    int vowels = 0;
    bool double_letter = false;
    const char *disallowed[] = {"ab", "cd", "pq", "xy"};

    // Check for at least three vowels
    for (int i = 0; str[i]; i++) {
        if (strchr("aeiou", str[i]) != NULL) vowels++;
    }

    // Check for at least one letter that appears twice in a row
    for (int i = 0; str[i] && str[i + 1]; i++) {
        if (str[i] == str[i + 1]) {
            double_letter = true;
            break;
        }
    }

    // Check for disallowed strings
    for (int i = 0; i < 4; i++) {
        if (strstr(str, disallowed[i]) != NULL) return false;
    }

    return vowels >= 3 && double_letter;
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
