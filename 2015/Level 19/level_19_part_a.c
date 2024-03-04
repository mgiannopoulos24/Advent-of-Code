#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_REPLACEMENTS 1000
#define MAX_MOLECULE_LENGTH 1000

typedef struct {
    char source[100];
    char target[100];
} Replacement;

int generate_molecules(Replacement replacements[], char molecule[]) {
    char new_molecule[MAX_MOLECULE_LENGTH];
    int num_replacements = 0;
    int molecule_length = strlen(molecule);

    for (int i = 0; i < molecule_length; i++) {
        for (int j = 0; j < num_replacements; j++) {
            int source_length = strlen(replacements[j].source);
            if (strncmp(molecule + i, replacements[j].source, source_length) == 0) {
                strcpy(new_molecule, molecule);
                strcpy(new_molecule + i, replacements[j].target);
                strcpy(new_molecule + i + strlen(replacements[j].target), molecule + i + source_length);
                strcpy(molecule, new_molecule);
                num_replacements++;
            }
        }
    }

    return num_replacements;
}

int main() {
    FILE *file = fopen("input_level_19.txt", "r");
    if (file == NULL) {
        printf("Error opening file.\n");
        return 1;
    }

    Replacement replacements[MAX_REPLACEMENTS];
    char molecule[MAX_MOLECULE_LENGTH];
    int num_replacements = 0;

    char line[100];
    while (fgets(line, sizeof(line), file) != NULL) {
        if (line[strlen(line) - 1] == '\n') {
            line[strlen(line) - 1] = '\0';
        }

        if (strcmp(line, "") == 0) {
            break;
        }

        char *source = strtok(line, " ");
        char *target = strtok(NULL, " ");
        strcpy(replacements[num_replacements].source, source);
        strcpy(replacements[num_replacements].target, target);
        num_replacements++;
    }

    fgets(molecule, sizeof(molecule), file);
    if (molecule[strlen(molecule) - 1] == '\n') {
        molecule[strlen(molecule) - 1] = '\0';
    }

    fclose(file);

    int distinct_molecules = generate_molecules(replacements, molecule);
    printf("%d\n", distinct_molecules);

    return 0;
}
