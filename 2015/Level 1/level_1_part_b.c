#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
    FILE *f = fopen("input_level_1.txt", "r");
    if (f == NULL) {
        printf("Error opening file\n");
        return 1;
    }

    int floor = 0;
    int position = 0; // Initialize position counter
    char c;

    // Read each character from the file until EOF
    while ((c = fgetc(f)) != EOF) {
        position++; // Increment position for each character read

        if (c == '(') {
            floor++; // Go up one floor for each opening parenthesis
        } else if (c == ')') {
            floor--; // Go down one floor for each closing parenthesis
        }

        // Check if Santa has entered the basement
        if (floor == -1) {
            printf("Santa enters the basement at character position: %d\n", position);
            break; // Exit the loop as we've found the position
        }
    }

    fclose(f); // Close the file
    return 0;
}
