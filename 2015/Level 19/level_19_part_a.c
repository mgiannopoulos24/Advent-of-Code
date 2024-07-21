#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINE_LENGTH 100
#define MAX_REPLACEMENTS 50
#define MAX_MOLECULE_LENGTH 1000

typedef struct {
    char source[MAX_LINE_LENGTH];
    char target[MAX_LINE_LENGTH];
} Replacement;

typedef struct Node {
    char molecule[MAX_MOLECULE_LENGTH];
    struct Node *next;
} Node;

void insert(Node **head, const char *molecule) {
    Node *new_node = (Node *)malloc(sizeof(Node));
    strcpy(new_node->molecule, molecule);
    new_node->next = *head;
    *head = new_node;
}

int contains(Node *head, const char *molecule) {
    Node *current = head;
    while (current != NULL) {
        if (strcmp(current->molecule, molecule) == 0) {
            return 1;
        }
        current = current->next;
    }
    return 0;
}

void free_list(Node *head) {
    Node *tmp;
    while (head != NULL) {
        tmp = head;
        head = head->next;
        free(tmp);
    }
}

void generate_molecules(Replacement *replacements, int rep_count, const char *molecule, Node **molecules) {
    int mol_len = strlen(molecule);

    for (int i = 0; i < mol_len; i++) {
        for (int j = 0; j < rep_count; j++) {
            int src_len = strlen(replacements[j].source);
            if (strncmp(&molecule[i], replacements[j].source, src_len) == 0) {
                char new_molecule[MAX_MOLECULE_LENGTH];
                strncpy(new_molecule, molecule, i);
                new_molecule[i] = '\0';
                strcat(new_molecule, replacements[j].target);
                strcat(new_molecule, &molecule[i + src_len]);
                if (!contains(*molecules, new_molecule)) {
                    insert(molecules, new_molecule);
                }
            }
        }
    }
}

int main() {
    FILE *file = fopen("input_level_19.txt", "r");
    if (file == NULL) {
        perror("Failed to open file");
        return 1;
    }

    Replacement replacements[MAX_REPLACEMENTS];
    int rep_count = 0;
    char line[MAX_LINE_LENGTH];

    while (fgets(line, sizeof(line), file)) {
        if (strchr(line, '=') == NULL) {
            break;
        }
        sscanf(line, "%s => %s", replacements[rep_count].source, replacements[rep_count].target);
        rep_count++;
    }

    char molecule[MAX_MOLECULE_LENGTH];
    fgets(molecule, sizeof(molecule), file);
    molecule[strcspn(molecule, "\n")] = 0;  // Remove newline character

    fclose(file);

    Node *molecules = NULL;
    generate_molecules(replacements, rep_count, molecule, &molecules);

    int distinct_molecules_count = 0;
    Node *current = molecules;
    while (current != NULL) {
        distinct_molecules_count++;
        current = current->next;
    }

    printf("Number of distinct molecules: %d\n", distinct_molecules_count);

    free_list(molecules);

    return 0;
}
