#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LOCATIONS 10
#define MAX_DISTANCES (MAX_LOCATIONS * (MAX_LOCATIONS - 1) / 2)
#define MAX_LOCATION_NAME_LENGTH 50
#define INF 10000000

typedef struct {
    char src[MAX_LOCATION_NAME_LENGTH];
    char dest[MAX_LOCATION_NAME_LENGTH];
    int distance;
} Distance;

Distance distances[MAX_DISTANCES];
char locations[MAX_LOCATIONS][MAX_LOCATION_NAME_LENGTH];
int numDistances = 0;
int numLocations = 0;

void add_location(const char* location) {
    for (int i = 0; i < numLocations; i++) {
        if (strcmp(locations[i], location) == 0) {
            return;
        }
    }
    strncpy(locations[numLocations], location, MAX_LOCATION_NAME_LENGTH - 1);
    locations[numLocations][MAX_LOCATION_NAME_LENGTH - 1] = '\0';
    numLocations++;
}

int find_distance(const char* src, const char* dest) {
    for (int i = 0; i < numDistances; i++) {
        if ((strcmp(distances[i].src, src) == 0 && strcmp(distances[i].dest, dest) == 0) ||
            (strcmp(distances[i].src, dest) == 0 && strcmp(distances[i].dest, src) == 0)) {
            return distances[i].distance;
        }
    }
    return -1;
}

void swap(char (*a)[MAX_LOCATION_NAME_LENGTH], char (*b)[MAX_LOCATION_NAME_LENGTH]) {
    char temp[MAX_LOCATION_NAME_LENGTH];
    strcpy(temp, *a);
    strcpy(*a, *b);
    strcpy(*b, temp);
}

void calculate_and_update_longest_distance(char array[][MAX_LOCATION_NAME_LENGTH], int start, int end, int* longest_distance) {
    if (start == end) {
        int current_distance = 0;
        for (int i = 0; i < end; i++) {
            int distance = find_distance(array[i], array[i + 1]);
            if (distance == -1) return; // Invalid route
            current_distance += distance;
        }
        if (current_distance > *longest_distance) {
            *longest_distance = current_distance;
        }
        return;
    }
    for (int i = start; i <= end; i++) {
        swap(&array[start], &array[i]);
        calculate_and_update_longest_distance(array, start + 1, end, longest_distance);
        swap(&array[start], &array[i]); // backtrack
    }
}

void read_input_file(const char* file_path) {
    FILE* file = fopen(file_path, "r");
    if (file == NULL) {
        perror("Error opening file");
        return;
    }
    char line[100];
    while (fgets(line, sizeof(line), file)) {
        char src[MAX_LOCATION_NAME_LENGTH], dest[MAX_LOCATION_NAME_LENGTH];
        int distance;
        if (sscanf(line, "%s to %s = %d", src, dest, &distance) == 3) {
            add_location(src);
            add_location(dest);
            strncpy(distances[numDistances].src, src, MAX_LOCATION_NAME_LENGTH - 1);
            strncpy(distances[numDistances].dest, dest, MAX_LOCATION_NAME_LENGTH - 1);
            distances[numDistances].distance = distance;
            numDistances++;
        } else {
            printf("Skipping malformed line: %s", line);
        }
    }
    fclose(file);
}

int main() {
    const char* file_path = "input_level_9.txt";
    read_input_file(file_path);

    int longest_distance = 0;
    calculate_and_update_longest_distance(locations, 0, numLocations - 1, &longest_distance);

    printf("The longest distance is: %d\n", longest_distance);

    return 0;
}
