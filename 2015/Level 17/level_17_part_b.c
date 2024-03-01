#include <stdio.h>
#include <stdlib.h>

#define MAX_CONTAINERS 100

// Function to read input from file
int read_input(char *file_name, int *containers) {
    FILE *file = fopen(file_name, "r");
    if (file == NULL) {
        perror("Error opening file");
        exit(EXIT_FAILURE);
    }

    int num_containers = 0;
    while (fscanf(file, "%d", &containers[num_containers]) != EOF) {
        num_containers++;
    }

    fclose(file);
    return num_containers;
}

// Function to calculate combinations
int count_combinations(int *containers, int num_containers, int target, int index, int num_used) {
    // If target becomes 0 and we have used minimum number of containers, then a combination is found
    if (target == 0 && num_used > 0) {
        return 1;
    }

    // If all containers are checked or target becomes negative
    if (index == num_containers || target < 0 || num_used == 0) {
        return 0;
    }

    // Recursive call by including the current container and excluding it
    return count_combinations(containers, num_containers, target - containers[index], index + 1, num_used - 1) +
           count_combinations(containers, num_containers, target, index + 1, num_used);
}

int main() {
    // Path to the input file
    char *input_file = "input_level_17.txt";
    // Array to store container capacities
    int containers[MAX_CONTAINERS];
    // Read the input file and get the number of containers
    int num_containers = read_input(input_file, containers);
    // Target eggnog liters
    int target_eggnog = 150;
    
    // Find the minimum number of containers needed
    int min_containers = 1;
    while (count_combinations(containers, num_containers, target_eggnog, 0, min_containers) == 0) {
        min_containers++;
    }

    // Calculate the number of combinations with minimum containers
    int combinations = count_combinations(containers, num_containers, target_eggnog, 0, min_containers);

    printf("Minimum number of containers required: %d\n", min_containers);
    printf("Number of different combinations with minimum containers: %d\n", combinations);

    return 0;
}
