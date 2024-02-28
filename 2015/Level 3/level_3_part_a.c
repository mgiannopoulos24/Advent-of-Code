#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *file = fopen("input_level_3.txt", "r");
    if (file == NULL) {
        printf("Error opening file\n");
        return 1;
    }

    int x = 0, y = 0; // Santa's starting position
    int houses[10000][2]; // Array to store visited houses, adjust size as needed
    int count = 1; // Starting house receives a present

    // Initialize starting position
    houses[0][0] = x;
    houses[0][1] = y;

    char move;
    while ((move = fgetc(file)) != EOF) {
        // Update position based on the move
        switch (move) {
            case '^': y++; break;
            case 'v': y--; break;
            case '>': x++; break;
            case '<': x--; break;
        }

        // Check if the new position is already in the list
        int found = 0;
        for (int i = 0; i < count; i++) {
            if (houses[i][0] == x && houses[i][1] == y) {
                found = 1;
                break;
            }
        }

        // If the position is new, add it to the list
        if (!found) {
            houses[count][0] = x;
            houses[count][1] = y;
            count++;
        }
    }

    fclose(file);

    printf("Number of houses that receive at least one present: %d\n", count);

    return 0;
}
