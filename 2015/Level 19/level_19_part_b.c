#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_REPLACEMENTS 100
#define MAX_MOLECULE_LENGTH 1000

typedef struct {
    char source[5];  // Assuming the source is always a single element, e.g., 'e', 'H', etc.
    char target[MAX_MOLECULE_LENGTH];
} Replacement;

Replacement replacements[MAX_REPLACEMENTS];
int replacement_count = 0;
char molecule[MAX_MOLECULE_LENGTH];

// Function to compare replacements based on the length of the target string
int cmp(const void *a, const void *b) {
    const Replacement *ra = (const Replacement *)a;
    const Replacement *rb = (const Replacement *)b;
    return strlen(rb->target) - strlen(ra->target);
}

// Function to perform the replacement
bool replace(char *str, const char *from, const char *to, int start) {
    char buffer[MAX_MOLECULE_LENGTH];
    char *insert_point = &buffer[0];
    const char *tmp = str;
    size_t from_len = strlen(from), to_len = strlen(to);

    while (1) {
        const char *p = strstr(tmp, from);
        if (p == NULL) {
            strcpy(insert_point, tmp);
            break;
        }
        memcpy(insert_point, tmp, p - tmp);
        insert_point += p - tmp;
        memcpy(insert_point, to, to_len);
        insert_point += to_len;
        tmp = p + from_len;
    }

    // Successful replacement if the buffer is different from the original string
    bool replaced = (strcmp(str, buffer) != 0);
    strcpy(str, buffer);
    return replaced;
}

// Function to find the minimum number of steps to reduce the molecule to 'e'
int find_min_steps(char *molecule) {
    int steps = 0;
    while (strcmp(molecule, "e") != 0) {
        bool reduced = false;
        for (int i = 0; i < replacement_count; i++) {
            if (replace(molecule, replacements[i].target, replacements[i].source, 0)) {
                reduced = true;
                steps++;
                break;
            }
        }
        if (!reduced) {
            printf("Stuck, cannot reduce further.\n");
            return -1;
        }
    }
    return steps;
}

int main() {
    FILE *file = fopen("input_level_19.txt", "r");
    if (!file) {
        perror("Unable to open file");
        return 1;
    }

    char source[5], target[MAX_MOLECULE_LENGTH];
    while (fscanf(file, "%s => %s", source, target) == 2) {
        strcpy(replacements[replacement_count].source, source);
        strcpy(replacements[replacement_count].target, target);
        replacement_count++;
    }
    fscanf(file, "%s", molecule);  // Assuming the last line is the molecule

    // Sort the replacements to try reducing the molecule with the largest possible chunks first
    qsort(replacements, replacement_count, sizeof(Replacement), cmp);

    int steps = find_min_steps(molecule);
    if (steps >= 0) {
        printf("Minimum steps to create the molecule: %d\n", steps);
    }

    fclose(file);
    return 0;
}
