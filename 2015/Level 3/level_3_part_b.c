#include <stdio.h>
#include <stdlib.h>

#define MAX_HOUSES 20000 // Increase if necessary

int main() {
    FILE *file = fopen("input_level_3.txt", "r");
    if (file == NULL) {
        printf("Error opening file\n");
        return 1;
    }

    int santa_x = 0, santa_y = 0; // Santa's starting position
    int robo_x = 0, robo_y = 0; // Robo-Santa's starting position
    int houses[MAX_HOUSES][2]; // Array to store visited houses
    int count = 1; // Starting house receives two presents, but counted as one
    int turn = 0; // 0 for Santa, 1 for Robo-Santa

    // Initialize starting position
    houses[0][0] = 0;
    houses[0][1] = 0;

    char move;
    while ((move = fgetc(file)) != EOF) {
        if (turn == 0) { // Santa's turn
            switch (move) {
                case '^': santa_y++; break;
                case 'v': santa_y--; break;
                case '>': santa_x++; break;
                case '<': santa_x--; break;
            }
            turn = 1; // Next move is Robo-Santa's
        } else { // Robo-Santa's turn
            switch (move) {
                case '^': robo_y++; break;
                case 'v': robo_y--; break;
                case '>': robo_x++; break;
                case '<': robo_x--; break;
            }
            turn = 0; // Next move is Santa's
        }

        // Determine whose position to check
        int current_x = turn == 0 ? santa_x : robo_x;
        int current_y = turn == 0 ? santa_y : robo_y;

        // Check if the new position is already in the list
        int found = 0;
        for (int i = 0; i < count; i++) {
            if (houses[i][0] == current_x && houses[i][1] == current_y) {
                found = 1;
                break;
            }
        }

        // If the position is new, add it to the list
        if (!found) {
            houses[count][0] = current_x;
            houses[count][1] = current_y;
            count++;
        }
    }

    fclose(file);

    printf("Number of houses that receive at least one present: %d\n", count);

    return 0;
}
