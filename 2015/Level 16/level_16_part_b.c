#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_AUNTS 500
#define MAX_LINE_LEN 256

// Define a structure to store the attributes of each Aunt Sue
typedef struct {
    int number;
    int children;
    int cats;
    int samoyeds;
    int pomeranians;
    int akitas;
    int vizslas;
    int goldfish;
    int trees;
    int cars;
    int perfumes;
} AuntSue;

AuntSue aunts[MAX_AUNTS];
int aunt_count = 0;

// Function to parse an attribute value, returning -1 if not present
int parse_attribute(char *line, const char *attribute) {
    char *found = strstr(line, attribute);
    if (found) {
        return atoi(found + strlen(attribute) + 2); // Skip ": " after attribute name
    }
    return -1;
}

// Function to compare detected attributes with an Aunt's attributes
int matches(AuntSue aunt, int detected[]) {
    if (aunt.children != -1 && aunt.children != detected[0]) return 0;
    if (aunt.cats != -1 && aunt.cats <= detected[1]) return 0; // cats: greater than detected
    if (aunt.samoyeds != -1 && aunt.samoyeds != detected[2]) return 0;
    if (aunt.pomeranians != -1 && aunt.pomeranians >= detected[3]) return 0; // pomeranians: less than detected
    if (aunt.akitas != -1 && aunt.akitas != detected[4]) return 0;
    if (aunt.vizslas != -1 && aunt.vizslas != detected[5]) return 0;
    if (aunt.goldfish != -1 && aunt.goldfish >= detected[6]) return 0; // goldfish: less than detected
    if (aunt.trees != -1 && aunt.trees <= detected[7]) return 0; // trees: greater than detected
    if (aunt.cars != -1 && aunt.cars != detected[8]) return 0;
    if (aunt.perfumes != -1 && aunt.perfumes != detected[9]) return 0;
    return 1;
}

int main() {
    FILE *file = fopen("input_level_16.txt", "r");
    if (!file) {
        perror("Failed to open file");
        return EXIT_FAILURE;
    }

    char line[MAX_LINE_LEN];
    while (fgets(line, sizeof(line), file)) {
        AuntSue aunt = {0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1};
        sscanf(line, "Sue %d:", &aunt.number);
        aunt.children = parse_attribute(line, "children");
        aunt.cats = parse_attribute(line, "cats");
        aunt.samoyeds = parse_attribute(line, "samoyeds");
        aunt.pomeranians = parse_attribute(line, "pomeranians");
        aunt.akitas = parse_attribute(line, "akitas");
        aunt.vizslas = parse_attribute(line, "vizslas");
        aunt.goldfish = parse_attribute(line, "goldfish");
        aunt.trees = parse_attribute(line, "trees");
        aunt.cars = parse_attribute(line, "cars");
        aunt.perfumes = parse_attribute(line, "perfumes");
        aunts[aunt_count++] = aunt;
    }
    fclose(file);

    int detected[] = {3, 7, 2, 3, 0, 0, 5, 3, 2, 1};

    for (int i = 0; i < aunt_count; i++) {
        if (matches(aunts[i], detected)) {
            printf("Aunt Sue who gave the gift is number: %d\n", aunts[i].number);
            break;
        }
    }

    return EXIT_SUCCESS;
}
