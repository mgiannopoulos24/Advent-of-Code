#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_PEOPLE 10
#define MAX_NAME_LENGTH 50

int happiness[MAX_PEOPLE][MAX_PEOPLE] = {0};
char names[MAX_PEOPLE][MAX_NAME_LENGTH];
int num_names = 0;
int max_happiness = -1000000;
int optimal_arrangement[MAX_PEOPLE];

int find_name_index(const char *name) {
    for (int i = 0; i < num_names; i++) {
        if (strcmp(names[i], name) == 0) {
            return i;
        }
    }
    strcpy(names[num_names], name);
    return num_names++;
}

int calculate_happiness(int arrangement[], int num_people) {
    int total_happiness = 0;
    for (int i = 0; i < num_people; i++) {
        int left_neighbor = arrangement[(i + num_people - 1) % num_people];
        int right_neighbor = arrangement[(i + 1) % num_people];
        total_happiness += happiness[arrangement[i]][left_neighbor];
        total_happiness += happiness[arrangement[i]][right_neighbor];
    }
    return total_happiness;
}

void read_input(const char *filename) {
    FILE *file = fopen(filename, "r");
    if (!file) {
        perror("Error opening file");
        exit(EXIT_FAILURE);
    }
    char line[256];
    while (fgets(line, sizeof(line), file)) {
        char person1[MAX_NAME_LENGTH], person2[MAX_NAME_LENGTH], verb[5];
        int happiness_units;
        sscanf(line, "%s would %s %d happiness units by sitting next to %s.", person1, verb, &happiness_units, person2);
        person2[strlen(person2) - 1] = '\0'; // Remove the period
        if (strcmp(verb, "lose") == 0) {
            happiness_units = -happiness_units;
        }
        int index1 = find_name_index(person1);
        int index2 = find_name_index(person2);
        happiness[index1][index2] = happiness_units;
    }
    fclose(file);
}

void swap(int *a, int *b) {
    int temp = *a;
    *a = *b;
    *b = temp;
}

void find_all_arrangements(int arrangement[], int l, int r) {
    if (l == r) {
        int current_happiness = calculate_happiness(arrangement, num_names);
        if (current_happiness > max_happiness) {
            max_happiness = current_happiness;
            memcpy(optimal_arrangement, arrangement, sizeof(int) * num_names);
        }
    } else {
        for (int i = l; i <= r; i++) {
            swap(&arrangement[l], &arrangement[i]);
            find_all_arrangements(arrangement, l + 1, r);
            swap(&arrangement[l], &arrangement[i]);
        }
    }
}

int main() {
    read_input("input_level_13.txt");
    int arrangement[MAX_PEOPLE];
    for (int i = 0; i < num_names; i++) {
        arrangement[i] = i;
    }
    find_all_arrangements(arrangement, 1, num_names - 1);
    printf("The optimal arrangement has a total happiness of %d.\n", max_happiness);
    printf("Optimal seating: ");
    for (int i = 0; i < num_names; i++) {
        printf("%s ", names[optimal_arrangement[i]]);
    }
    printf("\n");
    return 0;
}
