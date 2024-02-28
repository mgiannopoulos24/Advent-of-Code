#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
    FILE *f = fopen("input_level_1.txt", "r"); // Correctly opens the file for reading
    if (f == NULL) {
        printf("Error opening file\n");
        return 1;
    }

    int floor = 0;
    char c; // Use a char to read each character from the file

    // Read each character from the file until EOF
    while ((c = fgetc(f)) != EOF) {
        if (c == '(') {
            floor += 1; // Go up one floor for each opening parenthesis
        } else if (c == ')') {
            floor -= 1; // Go down one floor for each closing parenthesis
        }
    }

    printf("The right floor is: %d\n", floor); // Print the final floor
    fclose(f); // Close the file
    return 0;
}
