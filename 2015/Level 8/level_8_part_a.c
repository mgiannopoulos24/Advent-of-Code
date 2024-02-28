#include <stdio.h>
#include <string.h>

int main() {
    FILE* file = fopen("input_level_8.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    char line[1024];
    int totalCodeChars = 0, totalMemoryChars = 0;

    while (fgets(line, sizeof(line), file)) {
        size_t lineLen = strlen(line) - 1; // Exclude newline character
        totalCodeChars += lineLen; // Total characters in code

        // Calculate in-memory length
        int memoryChars = 0;
        for (int i = 1; i < (int)lineLen - 1; i++) { // Exclude the surrounding quotes
            if (line[i] == '\\') { // Check for escape sequence
                if (line[i + 1] == '\\' || line[i + 1] == '\"') {
                    memoryChars++;
                    i++; // Skip next char as it's part of the escape sequence
                } else if (line[i + 1] == 'x') {
                    memoryChars++;
                    i += 3; // Skip next three chars (x and two hex digits)
                }
            } else {
                memoryChars++;
            }
        }
        totalMemoryChars += memoryChars;
    }

    fclose(file);

    printf("Total characters of code: %d\n", totalCodeChars);
    printf("Total characters in memory: %d\n", totalMemoryChars);
    printf("Difference: %d\n", totalCodeChars - totalMemoryChars);

    return 0;
}
