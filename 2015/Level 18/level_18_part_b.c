#include <stdio.h>
#include <string.h>

#define GRID_SIZE 100
#define NUM_STEPS 100

// Function to count the number of neighboring lights that are on
int count_neighbors_on(char grid[GRID_SIZE][GRID_SIZE], int x, int y) {
    int count = 0;
    for (int i = x - 1; i <= x + 1; i++) {
        for (int j = y - 1; j <= y + 1; j++) {
            if (i >= 0 && i < GRID_SIZE && j >= 0 && j < GRID_SIZE && !(i == x && j == y) && grid[i][j] == '#') {
                count++;
            }
        }
    }
    return count;
}

// Function to simulate one step of animation
void animate_grid(char grid[GRID_SIZE][GRID_SIZE]) {
    char new_grid[GRID_SIZE][GRID_SIZE];
    memcpy(new_grid, grid, sizeof(char) * GRID_SIZE * GRID_SIZE);

    for (int i = 0; i < GRID_SIZE; i++) {
        for (int j = 0; j < GRID_SIZE; j++) {
            if ((i == 0 || i == GRID_SIZE - 1) && (j == 0 || j == GRID_SIZE - 1)) {
                // Skip corner lights
                continue;
            }

            int neighbors_on = count_neighbors_on(grid, i, j);

            if (grid[i][j] == '#') { // If the light is on
                if (neighbors_on != 2 && neighbors_on != 3) {
                    new_grid[i][j] = '.';
                }
            } else { // If the light is off
                if (neighbors_on == 3) {
                    new_grid[i][j] = '#';
                }
            }
        }
    }

    memcpy(grid, new_grid, sizeof(char) * GRID_SIZE * GRID_SIZE);
}

int main() {
    // Read initial configuration from file
    FILE *file = fopen("input_level_18.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    char grid[GRID_SIZE][GRID_SIZE];
    for (int i = 0; i < GRID_SIZE; i++) {
        fscanf(file, "%s", grid[i]);
    }
    fclose(file);

    // Set corners to always on
    grid[0][0] = '#';
    grid[0][GRID_SIZE - 1] = '#';
    grid[GRID_SIZE - 1][0] = '#';
    grid[GRID_SIZE - 1][GRID_SIZE - 1] = '#';

    // Simulate animation for 100 steps
    for (int step = 0; step < NUM_STEPS; step++) {
        animate_grid(grid);
    }

    // Count number of lights that are on
    int lights_on = 0;
    for (int i = 0; i < GRID_SIZE; i++) {
        for (int j = 0; j < GRID_SIZE; j++) {
            if (grid[i][j] == '#') {
                lights_on++;
            }
        }
    }

    printf("Number of lights on after 100 steps: %d\n", lights_on);

    return 0;
}
