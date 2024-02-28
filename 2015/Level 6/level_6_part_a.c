#include <stdio.h>
#include <string.h>

#define GRID_SIZE 1000

void apply_instruction(int grid[GRID_SIZE][GRID_SIZE], const char *instruction) {
    int x1, y1, x2, y2;
    // Handle "turn on" instruction
    if (sscanf(instruction, "turn on %d,%d through %d,%d", &x1, &y1, &x2, &y2) == 4) {
        for (int x = x1; x <= x2; x++) {
            for (int y = y1; y <= y2; y++) {
                grid[x][y] = 1;
            }
        }
    }
    // Handle "turn off" instruction
    else if (sscanf(instruction, "turn off %d,%d through %d,%d", &x1, &y1, &x2, &y2) == 4) {
        for (int x = x1; x <= x2; x++) {
            for (int y = y1; y <= y2; y++) {
                grid[x][y] = 0;
            }
        }
    }
    // Handle "toggle" instruction
    else if (sscanf(instruction, "toggle %d,%d through %d,%d", &x1, &y1, &x2, &y2) == 4) {
        for (int x = x1; x <= x2; x++) {
            for (int y = y1; y <= y2; y++) {
                grid[x][y] = !grid[x][y]; // Flip the state
            }
        }
    }
}

int main() {
    int grid[GRID_SIZE][GRID_SIZE] = {0};
    char instruction[256];

    // Open the file
    FILE *file = fopen("input_level_6.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    // Read instructions from the file
    while (fgets(instruction, sizeof(instruction), file)) {
        apply_instruction(grid, instruction);
    }

    // Close the file
    fclose(file);

    // Count the lit lights
    int lit_count = 0;
    for (int x = 0; x < GRID_SIZE; x++) {
        for (int y = 0; y < GRID_SIZE; y++) {
            if (grid[x][y]) lit_count++;
        }
    }

    printf("Number of lights lit: %d\n", lit_count);
    return 0;
}
